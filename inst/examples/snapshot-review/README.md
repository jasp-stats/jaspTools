# Try snapshot review

Run this in R with the development version of jaspTools installed.

```r
source(system.file("examples/snapshot-review/demo.R", package = "jaspTools"))
demo <- snapshotReviewExample()
```

Each call copies the example tests and their accepted snapshots to a fresh temporary module. The tests use small result nodes based on the Invisibility Cloak data and run without launching JASP. The default run produces four intentional snapshot failures. The report opens in your browser in an interactive R session.

| Snapshot | Candidate change |
| --- | --- |
| Independent Samples T-Test | The statistic changes from −1.71345938… to −1.71346938…. Both round to −1.713 at three decimals. |
| Group Descriptives | Maximum replaces Valid, the minimum in the second row changes inside a typed cell, and the footnote changes. |
| Analysis summary | The HTML summary changes the mean with the cloak from 5.00 to 5.50. |
| Group means | The second bar rises from 5.00 to 5.50. |

Click a changed table, figure or HTML preview to see the candidate, then the
baseline, then the differences again. Use **Accept** or **Decline** above the
preview. Use **Comment only** below it to leave the decision open for the agent.
Export the decision file and apply it to the example copy in the same R session.

```r
jaspTools::applySnapshotReview(file.choose(), demo$modulePath)
testthat::test_dir(file.path(demo$modulePath, "tests", "testthat"))
```

Accepted snapshots pass on the rerun. Declined candidates and those with only
a comment remain available for investigation.

To browse all accepted snapshots from a passing run, start a fresh copy.

```r
clean <- snapshotReviewExample(changed = FALSE)
```

Use `open = FALSE` to generate a report without opening a browser. The module path and report are available as `demo$modulePath` and `demo$report`. The temporary modules last for the current R session.
