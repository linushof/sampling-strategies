
# Data sets

All data sets can also be downloaded in one `.zip` file from the
accompanying [OSF repository](https://osf.io/wcr5a/):

`.rds` files: <https://osf.io/7wh89>

`.csv` files: <https://osf.io/cebhy>

<table style="width:100%;">
<colgroup>
<col style="width: 15%" />
<col style="width: 51%" />
<col style="width: 32%" />
</colgroup>
<tbody>
<tr class="odd">
<td><h3 id="file">File</h3></td>
<td><h3 id="description">Description</h3></td>
<td><h3 id="checksum-sha-256">Checksum (SHA-256)</h3></td>
</tr>
<tr class="even">
<td><code>choice_problems.rds</code></td>
<td>Choice problems used for simulation</td>
<td><code>bb2aff492c66e89e72eaf20a1dd9f03b77803ffec676c5d81ca1672509881d77</code></td>
</tr>
<tr class="odd">
<td><code>simulation_roundwise.rds.bz2</code></td>
<td>Exhaustive simulation data, containing all sampled outcomes and
choices for round-wise comparisons</td>
<td><code>7582d9cb842a7c3774c357a3e3385adacd505ea11c86f76cadd4021e0d45cd1e</code></td>
</tr>
<tr class="even">
<td><code>simulation_summary.rds.bz2</code></td>
<td>Exhaustive simulation data, containing all sampled outcomes and
choices for summary comparisons</td>
<td><code>eb2ab99f2e1fd8652d6b9571a2da3d06b8c1d75d5cd70cab7312519f9150188b</code></td>
</tr>
<tr class="odd">
<td><code>choice_data.rds.bz2</code></td>
<td>Trial-Level summaries of sampling (e.g., number of sampled outcomes,
switching frequency) and choice data</td>
<td><code>4d2691366562400c02a2c80a73ddb8ecd85c59cedf97590867a4a88ef7764b65</code></td>
</tr>
<tr class="even">
<td><code>cpt_estimates.rds</code></td>
<td>Précis of CPT posterior distributions</td>
<td><code>75a2dcd742fd11e713a5873a06ba2877c3bf34d6b67b4f0cde112db1ad28ccba</code></td>
</tr>
<tr class="odd">
<td><code>cpt_posteriors.rds</code></td>
<td>MCMC samples of CPT posterior distributions</td>
<td><code>faa2a52535728e7e1f87f47691b8b305c847b9f3d6c082737042c56923f60eb4</code></td>
</tr>
</tbody>
</table>

# Checksums

Use the checksums, provided on this page and in the .zip files, to check
whether the downloaded data sets are up-to-date and uncorrupted. In `R`,
the `digest` package/function can be used to generate the checksum of
\`\`your’’ data using the SHA-256 hash function.

``` r
pacman::p_load(digest, crayon)

# create checksum
your_checksum <- digest(your_data, algo="sha256")

# check data set
if(your_checksum != original_checksum){
  warning("\u2716 Mismatch between your data and original data. Current checksum is: '", your_checksum, "'")
} else{cat(green("\u2713 Data validated. Your data matches the original data."))
```

If the checksums are not the same, you are not operating on the exact
same data set. Be aware that the checksums of data sets stored in
different file format (e.g., `.csv` and `.rds`) while otherwise being
identical, differ.
