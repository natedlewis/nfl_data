NFL Data
================
Nate Lewis

## Datasets

### Rosters

The ‘rosters’ dataset provides fantasy relevant player data and ids of
from 2010 to 2020. This dataset is primarily used to gather positional
data to join with stats. The ids provided may also be utilized to
further join outside data.

``` r
rosters
```

    ## # A tibble: 13,068 x 16
    ##    player_id season full_name position status years_exp birth_date height weight
    ##    <chr>      <dbl> <chr>     <chr>    <chr>  <lgl>     <date>     <chr>   <dbl>
    ##  1 00-00279…   2021 James De… FB       Inact… NA        1988-07-23 6-3       255
    ##  2 00-00281…   2021 Bruce Mi… FB       Active NA        1987-08-06 6-2       248
    ##  3 00-00282…   2021 Nick Bel… FB       Active NA        1989-05-12 6-1       250
    ##  4 00-00285…   2021 Patrick … FB       Injur… NA        1989-04-30 6-1       234
    ##  5 00-00290…   2021 Derrick … FB       Active NA        1990-10-18 6-0       233
    ##  6 00-00294…   2021 Jamize O… FB       Volun… NA        1989-04-17 6-1       240
    ##  7 00-00297…   2021 Austin J… FB       Inact… NA        1989-06-16 6-2       240
    ##  8 00-00298…   2021 Kyle Jus… FB       Active NA        1991-04-23 6-1       240
    ##  9 00-00299…   2021 Zach Line FB       Active NA        1990-04-26 6-1       233
    ## 10 00-00301…   2021 Tommy Bo… FB       Active NA        1990-09-10 6-1       246
    ## # … with 13,058 more rows, and 7 more variables: college <chr>,
    ## #   draft_year <dbl>, draft_round <dbl>, draft_pick <dbl>,
    ## #   fantasy_position <chr>, depth_chart_position <lgl>, nw_position <chr>

### Games

The ‘games’ dataset provides game data and results from 2010 to 2020.

``` r
games
```

    ## # A tibble: 12,248 x 10
    ##    season  week location team  opp   score spread_line team_coach    opp_coach  
    ##     <dbl> <dbl> <chr>    <chr> <chr> <chr>       <dbl> <chr>         <chr>      
    ##  1   1999     1 Away     ARI   PHI   25-24         3   Vince Tobin   Andy Reid  
    ##  2   1999     1 Home     PHI   ARI   24-25        -3   Andy Reid     Vince Tobin
    ##  3   1999     1 Away     BAL   LA    10-27         0   Brian Billick Dick Verme…
    ##  4   1999     1 Home     LA    BAL   27-10         0   Dick Vermeil  Brian Bill…
    ##  5   1999     1 Away     BUF   IND   14-31         3   Wade Phillips Jim Mora   
    ##  6   1999     1 Home     IND   BUF   31-14        -3   Jim Mora      Wade Phill…
    ##  7   1999     1 Away     CAR   NO    10-19        -3.5 George Seife… Mike Ditka 
    ##  8   1999     1 Home     NO    CAR   19-10         3.5 Mike Ditka    George Sei…
    ##  9   1999     1 Away     CIN   TEN   35-36        -9   Bruce Coslet  Jeff Fisher
    ## 10   1999     1 Home     TEN   CIN   36-35         9   Jeff Fisher   Bruce Cosl…
    ## # … with 12,238 more rows, and 1 more variable: game_id <chr>

### Weekly

The ‘weekly’ dataset provides weekly player stats, snaps, injury data,
and further advanced metrics that will be added down the road.

##### Weekly Stats

The ‘weekly\_stats’ dataset provides player stats by week from 2010 to
2020.

``` r
weekly_stats
```

    ## # A tibble: 55,961 x 107
    ##    player_id  season  week completions attempts passing_yards passing_tds
    ##    <chr>       <dbl> <dbl>       <dbl>    <dbl>         <dbl>       <dbl>
    ##  1 00-0019596   2020     1          23       36           239           2
    ##  2 00-0019596   2020     2          23       35           217           1
    ##  3 00-0019596   2020     3          25       38           297           3
    ##  4 00-0019596   2020     4          30       46           369           5
    ##  5 00-0019596   2020     5          25       41           253           1
    ##  6 00-0019596   2020     6          17       27           166           2
    ##  7 00-0019596   2020     7          33       45           369           4
    ##  8 00-0019596   2020     8          28       40           279           2
    ##  9 00-0019596   2020     9          22       38           209           0
    ## 10 00-0019596   2020    10          28       39           341           3
    ## # … with 55,951 more rows, and 100 more variables: interceptions <dbl>,
    ## #   sacks <dbl>, sack_yards <dbl>, sack_fumbles <dbl>, sack_fumbles_lost <dbl>,
    ## #   passing_air_yards <dbl>, passing_yards_after_catch <dbl>,
    ## #   passing_first_downs <dbl>, passing_epa <dbl>,
    ## #   passing_2pt_conversions <dbl>, dakota <dbl>, outside_rz_attempts <dbl>,
    ## #   rz_attempts <dbl>, ez_attempts <dbl>, ez_attempts_inside_rz <dbl>,
    ## #   ez_attempts_outside_rz <dbl>, deep_attempts <dbl>, short_attempts <dbl>,
    ## #   neutral_attempts <dbl>, garbadge_time_attempts <dbl>,
    ## #   rz_completion_pct <dbl>, ez_completion_pct <dbl>,
    ## #   deep_completion_pct <dbl>, short_completion_pct <dbl>,
    ## #   passing_yards_250 <dbl>, passing_yards_300 <dbl>, passing_yards_350 <dbl>,
    ## #   passing_yards_400 <dbl>, passing_tds_1 <dbl>, passing_tds_20 <dbl>,
    ## #   passing_tds_50 <dbl>, int_tds <dbl>, carries <dbl>, rushing_yards <dbl>,
    ## #   rushing_tds <dbl>, rushing_fumbles <dbl>, rushing_fumbles_lost <dbl>,
    ## #   rushing_first_downs <dbl>, rushing_epa <dbl>,
    ## #   rushing_2pt_conversions <dbl>, outside_rz_carries <dbl>, rz_carries <dbl>,
    ## #   inside_ten_carries <dbl>, inside_five_carries <dbl>, neutral_carries <dbl>,
    ## #   garbadge_time_carries <dbl>, rushing_yards_90 <dbl>,
    ## #   rushing_yards_100 <dbl>, rushing_yards_150 <dbl>, rushing_yards_200 <dbl>,
    ## #   rushing_tds_1 <dbl>, rushing_tds_10 <dbl>, rushing_tds_30 <dbl>,
    ## #   rushing_tds_50 <dbl>, receptions <dbl>, targets <dbl>,
    ## #   receiving_yards <dbl>, receiving_tds <dbl>, receiving_fumbles <dbl>,
    ## #   receiving_fumbles_lost <dbl>, receiving_air_yards <dbl>,
    ## #   receiving_yards_after_catch <dbl>, receiving_first_downs <dbl>,
    ## #   receiving_epa <dbl>, receiving_2pt_conversions <dbl>,
    ## #   outside_rz_targets <dbl>, rz_targets <dbl>, ez_targets <dbl>,
    ## #   ez_targets_inside_rz <dbl>, ez_targets_outside_rz <dbl>,
    ## #   deep_targets <dbl>, short_targets <dbl>, neutral_targets <dbl>,
    ## #   garbadge_time_targets <dbl>, catch_rate <dbl>, adot <dbl>,
    ## #   rz_catch_rate <dbl>, ez_catch_rate <dbl>, deep_catch_rate <dbl>,
    ## #   short_catch_rate <dbl>, receiving_yards_90 <dbl>,
    ## #   receiving_yards_100 <dbl>, receiving_yards_150 <dbl>,
    ## #   receiving_yards_200 <dbl>, receiving_tds_1 <dbl>, receiving_tds_20 <dbl>,
    ## #   receiving_tds_50 <dbl>, special_teams_tds <dbl>, special_teams_tds_1 <dbl>,
    ## #   special_teams_tds_10 <dbl>, special_teams_tds_30 <dbl>,
    ## #   special_teams_tds_50 <dbl>, touches <dbl>, opportunities <dbl>,
    ## #   total_yards <dbl>, total_tds <dbl>, combo_yards_100 <dbl>,
    ## #   combo_yards_150 <dbl>, fantasy_points <dbl>, fantasy_points_ppr <dbl>

##### Weekly Data

The ‘weekly\_data’ provides injury and snap data by week from 2010 to
2020.

``` r
weekly_data
```

    ## # A tibble: 281,461 x 17
    ##    player_id  season  week active_inactive game_designation started total_snaps
    ##    <chr>       <dbl> <dbl> <chr>           <chr>            <chr>         <dbl>
    ##  1 00-0026936   2012     2 Active          Healthy          NO                0
    ##  2 00-0026936   2012     9 Active          Healthy          NO               63
    ##  3 00-0026936   2012     4 Active          Healthy          NO               14
    ##  4 00-0026936   2012    15 Active          Healthy          YES              40
    ##  5 00-0026936   2012     8 Active          Healthy          NO               18
    ##  6 00-0026936   2012     5 Active          Healthy          NO               22
    ##  7 00-0026936   2012    13 Active          Probable         NO               10
    ##  8 00-0026936   2012     7 Active          Healthy          NO               35
    ##  9 00-0026936   2012     6 Active          Healthy          NO               22
    ## 10 00-0026936   2012    12 Out             Doubtful         NO                0
    ## # … with 281,451 more rows, and 10 more variables: offense_snaps <dbl>,
    ## #   offense_snap_rate <dbl>, special_teams_snaps <dbl>, location <chr>,
    ## #   opponent <chr>, score <chr>, spread_line <dbl>, team_coach <chr>,
    ## #   opp_coach <chr>, game_id <chr>

### Seasons

The ‘seasons’ dataset aggregates the ‘weekly’ dataset by season.

##### Season Stats

The ‘season\_stats’ dataset provides overall player stats by aggregating
the ‘weekly\_stats’ dataset.

``` r
colnames(weekly_stats)
```

    ##   [1] "player_id"                   "season"                     
    ##   [3] "week"                        "completions"                
    ##   [5] "attempts"                    "passing_yards"              
    ##   [7] "passing_tds"                 "interceptions"              
    ##   [9] "sacks"                       "sack_yards"                 
    ##  [11] "sack_fumbles"                "sack_fumbles_lost"          
    ##  [13] "passing_air_yards"           "passing_yards_after_catch"  
    ##  [15] "passing_first_downs"         "passing_epa"                
    ##  [17] "passing_2pt_conversions"     "dakota"                     
    ##  [19] "outside_rz_attempts"         "rz_attempts"                
    ##  [21] "ez_attempts"                 "ez_attempts_inside_rz"      
    ##  [23] "ez_attempts_outside_rz"      "deep_attempts"              
    ##  [25] "short_attempts"              "neutral_attempts"           
    ##  [27] "garbadge_time_attempts"      "rz_completion_pct"          
    ##  [29] "ez_completion_pct"           "deep_completion_pct"        
    ##  [31] "short_completion_pct"        "passing_yards_250"          
    ##  [33] "passing_yards_300"           "passing_yards_350"          
    ##  [35] "passing_yards_400"           "passing_tds_1"              
    ##  [37] "passing_tds_20"              "passing_tds_50"             
    ##  [39] "int_tds"                     "carries"                    
    ##  [41] "rushing_yards"               "rushing_tds"                
    ##  [43] "rushing_fumbles"             "rushing_fumbles_lost"       
    ##  [45] "rushing_first_downs"         "rushing_epa"                
    ##  [47] "rushing_2pt_conversions"     "outside_rz_carries"         
    ##  [49] "rz_carries"                  "inside_ten_carries"         
    ##  [51] "inside_five_carries"         "neutral_carries"            
    ##  [53] "garbadge_time_carries"       "rushing_yards_90"           
    ##  [55] "rushing_yards_100"           "rushing_yards_150"          
    ##  [57] "rushing_yards_200"           "rushing_tds_1"              
    ##  [59] "rushing_tds_10"              "rushing_tds_30"             
    ##  [61] "rushing_tds_50"              "receptions"                 
    ##  [63] "targets"                     "receiving_yards"            
    ##  [65] "receiving_tds"               "receiving_fumbles"          
    ##  [67] "receiving_fumbles_lost"      "receiving_air_yards"        
    ##  [69] "receiving_yards_after_catch" "receiving_first_downs"      
    ##  [71] "receiving_epa"               "receiving_2pt_conversions"  
    ##  [73] "outside_rz_targets"          "rz_targets"                 
    ##  [75] "ez_targets"                  "ez_targets_inside_rz"       
    ##  [77] "ez_targets_outside_rz"       "deep_targets"               
    ##  [79] "short_targets"               "neutral_targets"            
    ##  [81] "garbadge_time_targets"       "catch_rate"                 
    ##  [83] "adot"                        "rz_catch_rate"              
    ##  [85] "ez_catch_rate"               "deep_catch_rate"            
    ##  [87] "short_catch_rate"            "receiving_yards_90"         
    ##  [89] "receiving_yards_100"         "receiving_yards_150"        
    ##  [91] "receiving_yards_200"         "receiving_tds_1"            
    ##  [93] "receiving_tds_20"            "receiving_tds_50"           
    ##  [95] "special_teams_tds"           "special_teams_tds_1"        
    ##  [97] "special_teams_tds_10"        "special_teams_tds_30"       
    ##  [99] "special_teams_tds_50"        "touches"                    
    ## [101] "opportunities"               "total_yards"                
    ## [103] "total_tds"                   "combo_yards_100"            
    ## [105] "combo_yards_150"             "fantasy_points"             
    ## [107] "fantasy_points_ppr"

##### Season Data

The ‘season\_data’ dataset provides injury and snap data from 2010 to
2020.

``` r
season_data
```

    ## # A tibble: 19,270 x 15
    ##    player_id  season games total_snaps offense_snaps special_teams_snaps started
    ##    <chr>       <dbl> <dbl>       <dbl>         <dbl>               <dbl>   <dbl>
    ##  1 00-0000108   2012    16         175             0                 175       0
    ##  2 00-0000108   2013    16          85             0                  85       0
    ##  3 00-0000551   2012    16         563             0                 393       3
    ##  4 00-0000585   2012    16        1018             0                   7      15
    ##  5 00-0000585   2013    16         188             0                   0       3
    ##  6 00-0000741   2012    16        1177             4                 102      16
    ##  7 00-0000865   2012    16         129           129                   0       2
    ##  8 00-0001263   2012    16        1019          1017                   2      16
    ##  9 00-0001820   2012    16         505             0                  53      14
    ## 10 00-0004091   2012    16         138             0                 138       0
    ## # … with 19,260 more rows, and 8 more variables: start_rate <dbl>,
    ## #   active_rate <dbl>, healthy_rate <dbl>, qual_snap_rate <dbl>,
    ## #   total_snaps_per_game <dbl>, offense_snaps_per_game <dbl>,
    ## #   offense_snap_rate <dbl>, special_teams_snaps_per_game <dbl>
