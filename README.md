# Using Data Science in Baseball
In this project, I analyze PITCHf/x data for Max Scherzer's 2016 Cy Young season and his average 2011 season.  By using data science techniques in Python, this project outlines what major things Scherzer changed between the two seasons.  
Research includes: Pitch selection compared to batter stance, count, and inning; change in spin axis, vertical break, and horizontal break; comparing spin rate to velocity; location of pitches hit, swung and missed at, and location by pitch type; and more.

Users Guide
- Each PITCHf/x data set is saved as a .csv in the GitHub Repository
- Some variable names are modified and or removed to suit the purposes of this project
- All variable descriptions can be found here: https://fastballs.wordpress.com/2007/08/02/glossary-of-the-gameday-pitch-fields
- The variables ‘pfx_x’ and ‘pfx_z’ (horizontal and vertical break respectively) are measured in inches and does not account for gravity.
- The variable ‘zone_location’ is a 5x5 grid, incrementing horizontally, starting with zero.  Zone locations that should be called a strike by the umpire are listed in the variable ‘isGloveStrike’.
- Fastballs include cutters, two-seams, and four-seams; everything else is considered off-speed.
- For all boolean variables using 0 and 1: 0 = false, 1 = true.
- Strike Zone height is measured as the average for the entire data set.
