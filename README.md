netAddHealth

Michael Metcalf Bishop

Data Source: The Longitudinal Study of Adolescent Health

Purpose: Prepare data and create functions for social network analysis with R and igraph
         It is often convenient to apply functions to add health network data by school or by community.
         See the functions at the very end: makeIgraphList and makeVertexAttribList
         which can be easily and fruitfully modified 
         
Output: igraphList which is a list of igraph objects, one for each community
        net5 which is the the data imported into net4, plus new measures of centrality 
        
Note: Change the location of the data indicated in the first few lines of code.
      Then, if you already have the three listed packages installed, the remainder of the code may
      be executed at once.

Acknowledgements: I owe thanks to Joyce Tabor and James Moody for assistance but any errors are my own. 


Details:
    
    - Import SAS datasets: sfriend.xpt, Inschool.xpt, allwave1.xpt, network.xpt
    - Merge all into net4
    - Create new and better school id (sschlcde2) and community id (ncommid2)
      which rely, in part, on info from Joyce Tabor, not in the original datasets
    - Create indicators for types of nominations based on "special codes," whether
      nominator/nominee is inside/outside school and/or roster
    - Sum indicators to describe each respondent's pattern of nominations
    - Eliminate repeat nominations, some rough choices
    - Create improved male variable using 3 potentially conflicting datasources 
    - Create school means for various nomination type sum variables
    - Split net4 into list of dataframes based on community (ncommid2)
    - Create list of igraph objects from above using makeIgraphList
    - Create list of vertex/respondent attributes, e.g. measures of centrality
        with makeVertexAttribList
    - Merge data back into net4