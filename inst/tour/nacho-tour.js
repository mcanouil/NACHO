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
 *   the sidebar opens, then the step waits until the Shiny server is idle
 *   and no panel or sidebar is still opening (plots that build in the
 *   background do not hold the tour back),
 * - a step whose part of the app is missing is skipped, and the skip is read
 *   out with the next step; the skip comes after 1.5 s once Shiny is idle,
 *   or after 10 s at most while Shiny is busy,
 * - when keys or clicks arrive while a step waits, the last move wins, and a
 *   restart keeps the page the first tour started from,
 * - a threshold panel, a sidebar or a collapsed navbar that the tour opens is
 *   closed again when the tour moves on, so it does not cover the next steps
 *   on a narrow window,
 * - a live region, created when the page loads, reads "Step N of M: title"
 *   to screen readers,
 * - arrow keys typed in an input move the cursor, not the tour (driver.js
 *   listens on window, so the event stops at document),
 * - reduced motion turns off the animations,
 * - at the end, the page that was open comes back and focus goes to the
 *   Help menu, or to the navbar toggle when the Help menu sits in a navbar
 *   that the tour has just closed. The end runs from onDestroyStarted and from Done, not from
 *   onDestroyed, because driver.js calls onDestroyed only after an animation
 *   frame has run.
 * nacho-tour.css gives the card the colours of the Bootstrap theme.
 */
(function () {
  "use strict";

  var WAIT_MS = 10000;
  var SETTLE_MS = 1500;
  var POLL_MS = 100;
  var HELP_TOGGLE = "a.dropdown-toggle[data-value='help']";

  var tour = null;
  var startPage = null;
  var pendingSkip = "";
  var navigation = 0;
  var opened = [];

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
    var link = document.querySelector(
      '.navbar a.active[data-bs-toggle="tab"][data-value]'
    );
    return link ? link.getAttribute("data-value") : null;
  }

  function showPage(page) {
    if (!page || page === activePage()) {
      return;
    }
    var link = document.querySelector(
      '.navbar a[data-bs-toggle="tab"][data-value="' + CSS.escape(page) + '"]'
    );
    if (!link) {
      return;
    }
    if (window.bootstrap && window.bootstrap.Tab) {
      window.bootstrap.Tab.getOrCreateInstance(link).show();
    } else {
      link.click();
    }
    var menu = link.closest(".dropdown");
    var toggle = menu && menu.querySelector(".dropdown-toggle");
    if (toggle && window.bootstrap && window.bootstrap.Dropdown) {
      window.bootstrap.Dropdown.getOrCreateInstance(toggle).hide();
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
      opened.push({ toggle: button, container: button.closest(".accordion") });
    }
  }

  function closeOpenedAround(target) {
    var closed = [];
    opened = opened.filter(function (entry) {
      if (target && entry.container.contains(target)) {
        return true;
      }
      entry.toggle.click();
      closed.push(entry);
      return false;
    });
    return closed;
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
      opened.push({ toggle: toggle, container: sidebar });
    }
  }

  function openNavbarAround(target) {
    var collapse = target && target.closest(".navbar-collapse");
    if (!collapse || collapse.classList.contains("show")) {
      return;
    }
    var toggler = collapse.id
      ? document.querySelector(
          '[data-bs-target="#' + CSS.escape(collapse.id) + '"]'
        )
      : null;
    if (toggler && visible(toggler)) {
      toggler.click();
      opened.push({ toggle: toggler, container: collapse });
    }
  }

  function visible(element) {
    return !!element && element.getClientRects().length > 0;
  }

  function shinyIdle() {
    return (
      !document.documentElement.classList.contains("shiny-busy") &&
      !Array.prototype.some.call(
        document.querySelectorAll(
          ".collapsing, .bslib-sidebar-layout.transitioning"
        ),
        visible
      )
    );
  }

  function prepare(step) {
    showPage(step.data.page);
    var target = document.querySelector(step.element);
    closeOpenedAround(target);
    openPanel(step.data.open);
    target = document.querySelector(step.element);
    openSidebarAround(target);
    openNavbarAround(target);
    return new Promise(function (resolve) {
      var waited = 0;
      var settled = 0;
      (function check() {
        var target = document.querySelector(step.element);
        var idle = shinyIdle();
        settled = idle && !visible(target) ? settled + POLL_MS : 0;
        if (visible(target) && idle) {
          resolve(true);
        } else if (settled >= SETTLE_MS || waited >= WAIT_MS) {
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
    var navbar = closeOpenedAround(null).filter(function (entry) {
      return entry.container.classList.contains("navbar-collapse");
    })[0];
    showPage(startPage);
    announce(pendingSkip);
    pendingSkip = "";
    window.setTimeout(function () {
      var focus = navbar ? navbar.toggle : document.querySelector(HELP_TOGGLE);
      if (focus) {
        focus.focus();
      }
    }, 0);
  }

  function go(driverObj, from, direction, token) {
    if (token === undefined) {
      navigation += 1;
      token = navigation;
    }
    var steps = driverObj.getConfig().steps;
    var index = from + direction;
    if (index < 0 || index >= steps.length) {
      if (!driverObj.isActive()) {
        tour = null;
        pendingSkip = "";
        closeOpenedAround(null);
        showPage(startPage);
        announce("The tour could not start: no part of the app is ready.");
      } else if (index < 0) {
        show(driverObj, driverObj.getActiveIndex());
      } else {
        finish(driverObj);
      }
      return;
    }
    setButtons(driverObj, true);
    prepare(steps[index]).then(function (ready) {
      if (tour !== driverObj || token !== navigation) {
        return;
      }
      if (!ready) {
        pendingSkip +=
          "Step " +
          (index + 1) +
          " skipped: its part of the app is not available. ";
        go(driverObj, index, direction, token);
        return;
      }
      show(driverObj, index);
    });
  }

  function show(driverObj, index) {
    var steps = driverObj.getConfig().steps;
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
    } else {
      startPage = activePage();
    }
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

  function editable(element) {
    return (
      !!element &&
      (element.isContentEditable ||
        /^(INPUT|TEXTAREA|SELECT)$/.test(element.tagName))
    );
  }

  document.addEventListener("keyup", function (event) {
    if (
      tour &&
      (event.key === "ArrowLeft" || event.key === "ArrowRight") &&
      editable(event.target)
    ) {
      event.stopPropagation();
    }
  });

  document.addEventListener("DOMContentLoaded", function () {
    liveRegion();
    if (window.Shiny && window.Shiny.addCustomMessageHandler) {
      window.Shiny.addCustomMessageHandler("nacho-tour-start", start);
    }
  });
})();
