# render() checks title and author

    Code
      render(GSE74821, title = 1)
    Condition
      Error in `render()`:
      ! `title` must be a single non-empty string, not a number.

---

    Code
      render(GSE74821, author = c("a", "b"))
    Condition
      Error in `render()`:
      ! `author` must be a single non-empty string, not a character vector.

