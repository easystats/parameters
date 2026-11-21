# vis_recipe.cluster_analysis

    Code
      print(out)
    Output
      Layer 1
      --------
      Geom type: text
      data = [150 x 4]
      aes_string(
        x = 'x'
        y = 'y'
        label = 'label'
        color = 'Cluster'
      )
      
      Layer 2
      --------
      Geom type: point
      data = [4 x 3]
      aes_string(
        x = 'x'
        y = 'y'
        color = 'Cluster'
      )
      shape = '+'
      size = 10
      
      Layer 3
      --------
      Geom type: labs
      x = 'PCA - 1'
      y = 'PCA - 2'
      title = 'Clustering Solution'
      

