/*
 * nacho-tour: the guided tour of the NACHO app.
 *
 * Built on driver.js 1.9.0 by Kamran Ahmed (MIT licence,
 * https://github.com/kamranahmedse/driver.js), vendored without changes in
 * driver.js and driver.css next to this file.
 *
 * This file adds what the NACHO app needs on top of driver.js:
 * - steps come from R in a Shiny message ("nacho-tour-start"),
 * - before a step shows, the page switches, the threshold panel opens and
 *   the sidebar opens, then the step waits for Shiny to finish rendering,
 * - a step whose part of the app is missing is skipped, and the skip is read
 *   out with the next step,
 * - a live region reads "Step N of M: title" to screen readers,
 * - reduced motion turns off the animations,
 * - at the end, the page that was open comes back and focus goes to the
 *   Help menu. The end runs from onDestroyStarted and from Done, not from
 *   onDestroyed, because driver.js calls onDestroyed only after an animation
 *   frame has run.
 * nacho-tour.css gives the card the colours of the Bootstrap theme.
 */
(function () {
  "use strict";

  var WAIT_MS = 10000;
  var POLL_MS = 100;
  var HELP_TOGGLE = "a.dropdown-toggle[data-value='help']";

  var tour = null;
  var startPage = null;
  var pendingSkip = "";

  function escapeText(text) {
    var element = document.createElement("div");
    element.textContent =
      text === null || text === undefined ? "" : String(text);
    return element.innerHTML;
  }

  function liveRegion() {
    var region = document.getElementById("nacho-tour-status");
    if (!region) {
      region = document.createElement("div");
      region.id = "nacho-tour-status";
      region.className = "visually-hidden";
      region.setAttribute("aria-live", "polite");
      document.body.appendChild(region);
    }
    return region;
  }

  function announce(text) {
    liveRegion().textContent = text;
  }

  function activePage() {
    var link = document.querySelector(".navbar .nav-link.active[data-value]");
    return link ? link.getAttribute("data-value") : null;
  }

  function showPage(page) {
    if (!page || page === activePage()) {
      return;
    }
    var link = document.querySelector(
      '.navbar a[data-value="' + CSS.escape(page) + '"]'
    );
    if (link) {
      link.click();
    }
  }

  function openPanel(panel) {
    if (!panel) {
      return;
    }
    var button = document.querySelector(
      '.accordion-item[data-value="' +
        CSS.escape(panel) +
        '"] .accordion-button'
    );
    if (button && button.classList.contains("collapsed")) {
      button.click();
    }
  }

  function openSidebarAround(target) {
    var layout =
      target && target.closest(".bslib-sidebar-layout.sidebar-collapsed");
    var sidebar = layout && layout.querySelector(":scope > .sidebar");
    if (!sidebar || !sidebar.contains(target)) {
      return;
    }
    var toggle = layout.querySelector(":scope > .collapse-toggle");
    if (toggle) {
      toggle.click();
    }
  }

  function visible(element) {
    return !!element && element.getClientRects().length > 0;
  }

  function shinyIdle() {
    return (
      !document.documentElement.classList.contains("shiny-busy") &&
      !Array.prototype.some.call(
        document.querySelectorAll(".recalculating, .collapsing"),
        visible
      )
    );
  }

  function prepare(step) {
    showPage(step.data.page);
    openPanel(step.data.open);
    openSidebarAround(document.querySelector(step.element));
    return new Promise(function (resolve) {
      var waited = 0;
      (function check() {
        var target = document.querySelector(step.element);
        if (visible(target) && shinyIdle()) {
          resolve(true);
        } else if (waited >= WAIT_MS) {
          resolve(visible(target));
        } else {
          waited += POLL_MS;
          window.setTimeout(check, POLL_MS);
        }
      })();
    });
  }

  function setButtons(driverObj, disabled) {
    var popover = driverObj.getState("popover");
    if (popover) {
      popover.nextButton.disabled = disabled;
      popover.previousButton.disabled = disabled;
    }
  }

  function finish(driverObj) {
    if (tour !== driverObj) {
      return;
    }
    tour = null;
    driverObj.destroy();
    showPage(startPage);
    announce("");
    window.setTimeout(function () {
      var help = document.querySelector(HELP_TOGGLE);
      if (help) {
        help.focus();
      }
    }, 0);
  }

  function go(driverObj, from, direction) {
    var steps = driverObj.getConfig().steps;
    var index = from + direction;
    if (index < 0 || index >= steps.length) {
      if (!driverObj.isActive()) {
        tour = null;
        announce("The tour could not start: no part of the app is ready.");
      } else if (index < 0) {
        driverObj.moveTo(driverObj.getActiveIndex());
      } else {
        finish(driverObj);
      }
      return;
    }
    setButtons(driverObj, true);
    prepare(steps[index]).then(function (ready) {
      if (tour !== driverObj) {
        return;
      }
      if (!ready) {
        pendingSkip +=
          "Step " +
          (index + 1) +
          " skipped: its part of the app is not available. ";
        go(driverObj, index, direction);
        return;
      }
      if (driverObj.isActive()) {
        driverObj.moveTo(index);
      } else {
        driverObj.drive(index);
      }
      announce(
        pendingSkip +
          "Step " +
          (index + 1) +
          " of " +
          steps.length +
          ": " +
          steps[index].data.title
      );
      pendingSkip = "";
    });
  }

  function toDriverStep(step) {
    return {
      element: step.element,
      data: {
        title: step.title,
        page: step.page || null,
        open: step.open || null
      },
      popover: {
        title: escapeText(step.title),
        description: escapeText(step.description),
        side: step.side || undefined
      }
    };
  }

  function start(message) {
    if (!window.driver || !window.driver.js) {
      console.error(
        "nacho-tour: driver.js is not loaded, so the tour cannot start."
      );
      return;
    }
    if (tour) {
      var previous = tour;
      tour = null;
      previous.destroy();
    }
    startPage = activePage();
    pendingSkip = "";
    var reduce = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
    var steps = message.steps.map(toDriverStep);
    var driverObj = window.driver.js.driver({
      steps: steps,
      showProgress: true,
      progressText: "{{current}} of {{total}}",
      popoverClass: "nacho-tour",
      nextBtnText: "Next",
      prevBtnText: "Back",
      doneBtnText: "Done",
      closeBtnLabel: "Close the tour",
      animate: !reduce,
      smoothScroll: !reduce,
      overlayOpacity: 0.6,
      onNextClick: function (element, step, opts) {
        go(opts.driver, opts.driver.getActiveIndex(), 1);
      },
      onPrevClick: function (element, step, opts) {
        go(opts.driver, opts.driver.getActiveIndex(), -1);
      },
      onDestroyStarted: function (element, step, opts) {
        finish(opts.driver);
      }
    });
    tour = driverObj;
    go(driverObj, -1, 1);
  }

  document.addEventListener("DOMContentLoaded", function () {
    if (window.Shiny && window.Shiny.addCustomMessageHandler) {
      window.Shiny.addCustomMessageHandler("nacho-tour-start", start);
    }
  });
})();
