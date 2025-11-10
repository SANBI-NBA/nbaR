# Basic NBA table

A basic table with a purple heading bar. This data should be in the same
format as for the plot functions

## Usage

``` r
nba_tbl(DF)
```

## Arguments

- DF:

  The data frame that contains the data

## Examples

``` r
tbl <- nba_tbl(NBA_example_pro_data)

tbl
#> <table class="table table-striped table-hover" style="font-size: 1px; width: auto !important; margin-left: auto; margin-right: auto;">
#>  <thead>
#> <tr><th style="empty-cells: hide;border-bottom:hidden;" colspan="5"></th></tr>
#>   <tr>
#>    <th style="text-align:left;font-weight: bold;color: black !important;background-color: rgba(137, 155, 225, 255) !important;border: 1px solid black"> OVERALL types </th>
#>    <th style="text-align:right;font-weight: bold;color: black !important;background-color: rgba(137, 155, 225, 255) !important;border: 1px solid black"> Well Protected </th>
#>    <th style="text-align:right;font-weight: bold;color: black !important;background-color: rgba(137, 155, 225, 255) !important;border: 1px solid black"> Moderately Protected </th>
#>    <th style="text-align:right;font-weight: bold;color: black !important;background-color: rgba(137, 155, 225, 255) !important;border: 1px solid black"> Poorly Protected </th>
#>    <th style="text-align:right;font-weight: bold;color: black !important;background-color: rgba(137, 155, 225, 255) !important;border: 1px solid black"> Not Protected </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;background-color: white !important;border-left:1px solid;border-right:1px solid;"> subAntarctic </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 15 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 14 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 1 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 4 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;background-color: white !important;border-left:1px solid;border-right:1px solid;"> Coastal </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 65 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 28 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 52 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 41 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;background-color: white !important;border-left:1px solid;border-right:1px solid;"> Estuarine </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 4 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 8 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 7 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 3 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;background-color: white !important;border-left:1px solid;border-right:1px solid;"> Marine </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 47 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 62 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 22 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 19 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;background-color: white !important;border-left:1px solid;border-right:1px solid;"> Wetland </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 8 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 4 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 41 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 82 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;background-color: white !important;border-left:1px solid;border-right:1px solid;"> River </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 29 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 33 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 66 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 94 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;background-color: white !important;border-left:1px solid;border-right:1px solid;"> Inland Aquatic </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 37 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 37 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 107 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 176 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;background-color: white !important;border-left:1px solid;border-right:1px solid;"> Terrestrial </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 118 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 59 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 166 </td>
#>    <td style="text-align:right;background-color: white !important;border-left:1px solid;border-right:1px solid;"> 115 </td>
#>   </tr>
#> </tbody>
#> </table>
```
