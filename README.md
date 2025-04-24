# QB_WR_College_Connection 📊🏈

https://x.com/PranavP137/status/1915248314242785781

This project explores whether reuniting NFL quarterbacks with their former college wide receivers impacts QB performance—specifically looking at Expected Points Added (EPA) per play.

## Inspiration 💡
With growing buzz around potential college WR-QB reunions in the NFL Draft (e.g., Stroud & Emeka, Young & Jameson), I wanted to see if the data supports the idea that these reunions actually improve quarterback play.

## Project Structure
```
QB_WR_College_Connection/
├── Data Visuals/ # Contains all generated plots for the analysis 
│ ├── 1_Catches_Between_Teammates.png 
│ ├── 2_Change_QB_EPA_For_Teammates.png 
│ └── 3_Change_QB_EPA_For_First_Rounders.png 
│
└── qb_wr_connection.R # Main R script with data loading, cleaning, and visualization
└── theme.R # Stores NFL Analytics Theme
```

## Analysis Overview
- **Step 1:** Identify WR-QB duos who played together in both college and the NFL.
- **Step 2:** Compare college catch totals to NFL catch totals between each duo.
- **Step 3:** Evaluate change in QB EPA per play after reuniting with their college WR.
- **Step 4:** Compare this to general trends of QBs after their team drafts a 1st-round WR.

## Key Insights
- Most college duos don’t recreate high-volume production in the NFL.
- 4 of 5 QBs improved their EPA after reuniting with a former teammate.
- Reunions may offer a slight edge over simply drafting any 1st-round WR (avg EPA boost of 0.097 vs 0.044).
- However, the sample size is small—more data is needed for a definitive conclusion.
- Murray and Brown’s negative EPA skewed the average significantly.

## Visuals
- **1_Catches_Between_Teammates.png**: Compares catches in college vs. NFL for QB-WR duos.
- **2_Change_QB_EPA_For_Teammates.png**: Shows change in QB EPA after reuniting with a college WR.
- **3_Change_QB_EPA_For_First_Rounders.png**: Compares general QB EPA change after drafting a 1st-round WR.

## Tools Used
- **R** for data analysis
  - `ggplot2` and `ggrepel` for visuals
  - `dplyr` for wrangling
- **Data Sources**
  - [cfbfastR](https://github.com/sportsdataverse/cfbfastR)
  - [nflverse](https://github.com/nflverse/nflverse-data)
- Custom mapping for team colors to enhance visual clarity

## Author
**Pranav Pitchala**  
Twitter: [@pranavp137](https://x.com/PranavP137)
