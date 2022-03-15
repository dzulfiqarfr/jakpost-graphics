# Code and data behind my stories on The Jakarta Post

This repo contains the code and data to produce the analysis and graphics for my stories on The Jakarta Post (JakPost). Not all my stories are included.

The code in this repo improves the original analysis and data visualizations where necessary. So there are some differences in analysis results and charts between this repo and the published stories on JakPost. Find the details in the changelog section of the README in each story (or project) directory.


## Project organization

I organized this repo by year and then by story. The story directories are stored in chronological order by publication date.

In each story directory, you'll find mostly four subdirectories, namely data, doc, src and result. The contents of each subdirectory are as follows:  
- data: data (both raw and cleaned) for analysis;  
- doc: documents, including PDFs that contain tables or technical notes on the analysis;  
- src: code to analyze the data and visualize the results; and  
- result: analysis results in CSVs and graphics in SVGs and PNGs.


## Tools

I used a combination of R, Google Sheets, Tabula and Inkscape to work on these projects. (For Datawrapper charts on the published stories, I recreated them with ggplot2.)


## License

I publish the code in this repo under the [MIT license](LICENSE). The data and graphics produced using the code are published under the [CC-BY-NC license](https://creativecommons.org/licenses/by-nc/4.0/legalcode).

I will really appreciate it if you let me know when you use the code, data or graphics.
