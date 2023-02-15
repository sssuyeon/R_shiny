###############################################################################
require(shiny)
require(visNetwork)
require(ggplot2)
require(gridExtra)
###############################################################################
ui = fluidPage(
  titlePanel('Social Influence Model based on Edge Attraction'),
  sidebarLayout(

    ########## siderbarPanel ##########
    sidebarPanel(
      fileInput(inputId = 'file1',
                label = 'Edge list: ',
                placeholder = 'No file selected...'),
      fileInput(inputId = 'file2',
                label = 'Item Response: ',
                placeholder = 'No file selected...'),
      fileInput(inputId = 'file3',
                label = 'MU matrix: ',
                placeholder = 'No file selected...'),
      fileInput(inputId = 'file4',
                label = 'First actor latent position: ',
                placeholder = 'No file selected...'),
      fileInput(inputId = 'file5',
                label = 'Second actor latent position: ',
                placeholder = 'No file selected...'),
      fileInput(inputId = 'file6',
                label = 'Item latent position: ',
                placeholder = 'No file selected...'),
      fileInput(inputId = 'file7',
                label = 'Second radius: ',
                placeholder = 'No file selected...'),
      numericInput(inputId = 'numeric1',
                   label = 'Actor ID. Maximum is the number of actors.',
                   min = 1,
                   value = 1),
      width = 2
    ),

    ########## mainPanel ##########
    mainPanel(
      tabsetPanel(
        id = 'function',
        tabPanel('About', h3(textOutput('mytext0')), h3(textOutput('mytext0_1')),
                 h4(textOutput('mytext0_2')), h4(textOutput('mytext0_3')),
                 h4(textOutput('mytext0_4')), h4(textOutput('mytext0_5')),
                 h4(textOutput('mytext0_6')), h4(textOutput('mytext0_7')),
                 h4(textOutput('mytext0_8')),
                 h3(textOutput('mytext0_9')), h4(textOutput('mytext0_10')),
                 h4(textOutput('mytext0_11')), h4(textOutput('mytext0_12')),
                 h4(textOutput('mytext0_13')), h4(textOutput('mytext0_14')),
                 h4(textOutput('mytext0_15')), h4(textOutput('mytext0_16')),
                 h3(textOutput('mytext0_17'))
        ),
        tabPanel('Influencing Network', h4(textOutput('mytext1')), h5(textOutput('mytext1_1')),
                 visNetworkOutput('myplot1', width = 'auto', height = '600px')),
        tabPanel('Influenced Network', h4(textOutput('mytext2')), h5(textOutput('mytext2_1')),
                 visNetworkOutput('myplot2', width = 'auto', height = '600px')),
        tabPanel('Item Response Network', h4(textOutput('mytext3')), h5(textOutput('mytext3_1')),
                 visNetworkOutput('myplot3', width = 'auto', height = '600px'),
                 plotOutput('myplot3_1', width = 'auto')),
        tabPanel('Latent Position', h4(textOutput('mytext4')),
                 plotOutput('myplot4', width = 'auto')),
        tabPanel('Each Actor Plot', h4(textOutput('mytext5')), h5(textOutput('mytext5_1')),
                 plotOutput('myplot5', width = 'auto')),
        tabPanel('Influencing Rank', h4(textOutput('mytext6')),
                 dataTableOutput('myplot6')),
        tabPanel('Influenced Rank', h4(textOutput('mytext7')),
                 dataTableOutput('myplot7'))
        ), width = 10
      )
    )
  )

###############################################################################
server = function(input, output) {
  output$mytext0 = renderText({"This is application for Social Influence Model based on Edge Attraction."})
  output$mytext0_1 = renderText({"Please input :"})
  output$mytext0_2 = renderText({"- Edgelist : edgelist.csv"})
  output$mytext0_3 = renderText({"- Item Response : item_response.csv"})
  output$mytext0_4 = renderText({"- MU matrix: mu_matrix.csv"})
  output$mytext0_5 = renderText({"- First actor latent position: zest.csv"})
  output$mytext0_6 = renderText({"- Second actor latent position: aest.csv"})
  output$mytext0_7 = renderText({"- Item latent position: best.csv"})
  output$mytext0_8 = renderText({"- Second radius: radii2est.csv"})

  output$mytext0_9 = renderText({"Info :"})
  output$mytext0_10 = renderText({"1) Influencing Network : Size of node reflects the summation of influencing MU"})
  output$mytext0_11 = renderText({"2) Influenced Network : Size of node reflects the summation of influenced MU"})
  output$mytext0_12 = renderText({"3) Item Response Network : Size of square node reflects the proportion of positive response"})
  output$mytext0_13 = renderText({"4) Latent Position : Actor and Item latent position at two points"})
  output$mytext0_14 = renderText({"5) Each Actor Plot : The width of arrow is proportional to the exponential of MU"})
  output$mytext0_15 = renderText({"6) Influencing Rank : Each actor and most 5 influenced actors by him or her"})
  output$mytext0_16 = renderText({"7) Influenced Rank : Each actor and most 5 influencing actors by him or her"})
  output$mytext0_17 = renderText({"Need to install following libraries: <shiny>, <visNetwork>, <ggplot2>, <gridExtra>"})

  output$mytext1 = renderText({"Network Visulaization Reflecting the Influencing Degree"})
  output$mytext1_1 = renderText({"Click each node to show that node and neighbor of him or her."})
  output$mytext2 = renderText({"Network Visulaization Reflecting the Influenced Degree"})
  output$mytext2_1 = renderText({"Click each node to show that node and neighbor of him or her."})
  output$mytext3 = renderText({"Network Visualization Reflecting the proportion of positive response"})
  output$mytext3_1 = renderText({"Click each node to show that node and neighbor of him or her."})
  output$mytext4 = renderText({"First/Second Latent Position Plot"})
  output$mytext5 = renderText({"Each Actor's Influencing Plot"})
  output$mytext5_1 = renderText({"The width of arrow is proportional to the exponential of MU."})
  output$mytext6 = renderText({"Each actor and most 5 influenced actors by him or her"})
  output$mytext7 = renderText({"Each actor and most 5 influencing actors to him or her"})

  ########## plot1 : Influencing Network ##########
  output$myplot1 = renderVisNetwork({
    edgelist.file = input$file1
    ext <- tools::file_ext(edgelist.file$datapath)
    req(edgelist.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    edgelist = as.matrix(read.csv(edgelist.file$datapath))

    mu_matrix.file = input$file3
    ext <- tools::file_ext(mu_matrix.file$datapath)
    req(mu_matrix.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    mu_matrix = as.matrix(read.csv(mu_matrix.file$datapath))

    nodes = data.frame(id = 1:dim(mu_matrix)[1], value = apply(mu_matrix, 1, sum),
                       label = paste('', 1:dim(mu_matrix)[1]))
    edges = data.frame(from = edgelist[,1], to = edgelist[,2])
    visNetwork(nodes, edges, height = '1000px', width = "100%") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "magenta")) %>%
      visLayout(randomSeed = 12)
  })

  ########## plot2 : Influenced Network ##########
  output$myplot2 = renderVisNetwork({
    edgelist.file = input$file1
    ext <- tools::file_ext(edgelist.file$datapath)
    req(edgelist.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    edgelist = as.matrix(read.csv(edgelist.file$datapath))

    mu_matrix.file = input$file3
    ext <- tools::file_ext(mu_matrix.file$datapath)
    req(mu_matrix.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    mu_matrix = as.matrix(read.csv(mu_matrix.file$datapath))

    nodes = data.frame(id = 1:dim(mu_matrix)[1], value = apply(mu_matrix, 2, sum),
                       label = paste('', 1:dim(mu_matrix)[1]))
    edges = data.frame(from = edgelist[,1], to = edgelist[,2])
    visNetwork(nodes, edges, height = '1000px', width = "100%") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               color = list(color = "lightblue", highlight = "magenta")) %>%
      visLayout(randomSeed = 12)
  })

  ########## plot3 : Item Response Plot ##########
  output$myplot3 = renderVisNetwork({
    itres.file = input$file2
    ext <- tools::file_ext(itres.file$datapath)
    req(itres.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    itres_matrix = as.matrix(read.csv(itres.file$datapath))

    items_df <- data.frame(which(itres_matrix == 1, arr.ind = TRUE))
    items_df[,2] = items_df[,2] + dim(itres_matrix)[1]
    nodes = data.frame(id = 1:(dim(itres_matrix)[1] + dim(itres_matrix)[2]),
                       group = c(rep('actor', dim(itres_matrix)[1]), rep('item', dim(itres_matrix)[2])),
                       label = c(paste('', 1:dim(itres_matrix)[1]), paste('item', 1:dim(itres_matrix)[2])),
                       value = c(rep(1, dim(itres_matrix)[1]), as.numeric(apply(itres_matrix, 2, sum))))
    edges = data.frame(from = items_df[,1], to = items_df[,2])
    visNetwork(nodes, edges, height = '1000px', width = "100%") %>%
      visGroups(groupname = 'actor', shape = 'dot') %>%
      visGroups(groupname = 'item', shape = 'square',
                color = list(background = 'pink', border = 'magenta', highlight = 'pink')) %>%
      visOptions(highlightNearest = TRUE) %>%
      visEdges(color = list(color = "lightblue", highlight = "magenta")) %>%
      visLayout(randomSeed = 12)
  })

  output$myplot3_1 = renderPlot({
    itres.file = input$file2
    ext <- tools::file_ext(itres.file$datapath)
    req(itres.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    itres_matrix = as.matrix(read.csv(itres.file$datapath))

    prob_df <- data.frame(id = 1:dim(itres_matrix)[2],
                          probs = as.numeric(apply(itres_matrix, 2, sum)/dim(itres_matrix)[1]))
    ggplot(prob_df, aes(x = rownames(prob_df), y = probs)) +
      geom_bar(stat = "identity", fill = 'lightblue') +
      labs(title="Plot of proportion item response 1", x ="Item", y = "proportion") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_discrete(limits=c(1:dim(prob_df)[1]))
    })


  ########## plot4: First/Second Latent Position ##########
  output$myplot4 = renderPlot({
    z.file = input$file4
    ext <- tools::file_ext(z.file$datapath)
    req(z.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    z.est = as.matrix(read.csv(z.file$datapath))

    a.file = input$file5
    ext <- tools::file_ext(a.file$datapath)
    req(a.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    a.est = as.matrix(read.csv(a.file$datapath))

    b.file = input$file6
    ext <- tools::file_ext(b.file$datapath)
    req(b.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    b.est = as.matrix(read.csv(b.file$datapath))

    mymin = 1.1*min(z.est, a.est, b.est); mymax = 1.1*max(z.est, a.est, b.est)
    # First
    temp1 = rbind(z.est, b.est); temp1 = as.data.frame(temp1)
    temp1 = cbind(temp1, c(rep('actor', dim(z.est)[1]), rep('item', dim(b.est)[1])))
    colnames(temp1) = c('actor', 'item', 'legend')
    g1 <- ggplot(temp1) +
      geom_point(aes(x = actor, y = item, colour = legend, shape = legend, size = legend)) + xlab('') + ylab('') +
      scale_x_continuous(limits = c(mymin, mymax)) +
      scale_y_continuous(limits = c(mymin, mymax)) +
      annotate('text', x = z.est[,1], y = z.est[,2] - 0.02, label = 1:dim(z.est)[1], size = 4) +
      coord_fixed(ratio = 1) +
      labs(title = 'First latent position') + theme(plot.title = element_text(hjust = 0.5)) +
      annotate('text', x = b.est[,1], y = b.est[,2] - 0.02, label = 1:dim(b.est)[1], size = 4, color = 'blue') +
      scale_color_manual(values = c('actor' = 'black', 'item' = 'blue')) +
      scale_shape_manual(values = c('actor' = 1, 'item' = 17)) +
      scale_size_manual(values = c('actor' = 2, 'item' = 3))
    # Second
    temp2 = rbind(a.est, b.est); temp2 = as.data.frame(temp2)
    temp2 = cbind(temp2, c(rep('actor', dim(a.est)[1]), rep('item', dim(b.est)[1])))
    colnames(temp2) = c('actor', 'item', 'legend')
    g2 <- ggplot(temp2) +
      geom_point(aes(x = actor, y = item, colour = legend, shape = legend, size = legend)) + xlab('') + ylab('') +
      scale_x_continuous(limits = c(mymin, mymax)) +
      scale_y_continuous(limits = c(mymin, mymax)) +
      annotate('text', x = a.est[,1], y = a.est[,2] - 0.02, label = 1:dim(a.est)[1], size = 4) +
      coord_fixed(ratio = 1) +
      labs(title = 'Second latent position') + theme(plot.title = element_text(hjust = 0.5)) +
      annotate('text', x = b.est[,1], y = b.est[,2] - 0.02, label = 1:dim(b.est)[1], size = 4, color = 'blue') +
      scale_color_manual(values = c('actor' = 'black', 'item' = 'blue')) +
      scale_shape_manual(values = c('actor' = 1, 'item' = 17)) +
      scale_size_manual(values = c('actor' = 2, 'item' = 3))

    grid.arrange(g1, g2, ncol = 2)
  }, width = 1000, height = 700)

  ########## plot5 : Each Actor Plot ##########
  output$myplot5 = renderPlot({
    z.file = input$file4
    ext <- tools::file_ext(z.file$datapath)
    req(z.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    z.est = as.matrix(read.csv(z.file$datapath))

    a.file = input$file5
    ext <- tools::file_ext(a.file$datapath)
    req(a.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    a.est = as.matrix(read.csv(a.file$datapath))

    b.file = input$file6
    ext <- tools::file_ext(b.file$datapath)
    req(b.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    b.est = as.matrix(read.csv(b.file$datapath))

    mu_matrix.file = input$file3
    ext <- tools::file_ext(mu_matrix.file$datapath)
    req(mu_matrix.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    mu_matrix = as.matrix(read.csv(mu_matrix.file$datapath))

    radii2.file = input$file7
    ext <- tools::file_ext(radii2.file$datapath)
    req(radii2.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    radii2.est = as.matrix(read.csv(radii2.file$datapath))

    k = as.numeric(input$numeric1)
    mymin = 1.5*min(z.est, a.est, b.est)
    mymax = 1.5*max(z.est, a.est, b.est)
    temp = which(mu_matrix[k,] > 0)
    tempmu = round(as.numeric(mu_matrix[k,][temp]), 3)

    ggplot() +
      geom_point(mapping = aes(x = a.est[k,1], y = a.est[k,2]), size = 4) + xlab('') + ylab('') +
      scale_x_continuous(limits = c(mymin, mymax)) +
      scale_y_continuous(limits = c(mymin, mymax)) +
      annotate('text', x = a.est[k,1], y = a.est[k,2] - 0.03, label = k, size = 5) +
      coord_fixed(ratio = 1) +
      labs(title = paste('influencing actor=', k, sep = '')) +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_point(mapping = aes(x = z.est[temp,1], y = z.est[temp,2]), size = 1, color = 'magenta') +
      geom_point(mapping = aes(x = a.est[temp,1], y = a.est[temp,2]), size = 1, color = 'magenta') +
      geom_segment(aes(x = z.est[temp,1], y = z.est[temp,2], xend = a.est[temp,1], yend = a.est[temp,2]),
                   arrow = arrow(length = unit(0.5, 'cm')), color = 'magenta', size = exp(tempmu)) +
      annotate('text', x = z.est[temp,1], y = z.est[temp,2] - 0.015, label = temp, size = 4, color = 'magenta') +
      annotate('text', x = (z.est[temp,1]+a.est[temp,1])/2 + 0.015, y = (z.est[temp,2]+a.est[temp,2])/2 + 0.015,
               label = tempmu, size = 4, color = 'magenta') +
      geom_point(mapping = aes(x = b.est[,1], y = b.est[,2]), shape = 17, size = 4, color = 'blue') +
      annotate('text', x = b.est[,1], y = b.est[,2] - 0.02, label = 1:dim(b.est)[1], size = 4, color = 'blue') +
      annotate('path', x = a.est[k,1] + radii2.est[k]*cos(seq(0, 2*pi, length.out = 100)),
               y = a.est[k,2] + radii2.est[k]*sin(seq(0, 2*pi, length.out = 100)))

  }, width = 700, height = 700)

  ########## plot6 : Influencing Rank ##########
  output$myplot6 = renderDataTable({
    mu_matrix.file = input$file3
    ext <- tools::file_ext(mu_matrix.file$datapath)
    req(mu_matrix.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    mu_matrix = as.matrix(read.csv(mu_matrix.file$datapath))

    ordermat = apply(mu_matrix, 1, order, decreasing = TRUE)[1:5,]
    sortmat = round(apply(mu_matrix, 1, sort, decreasing = TRUE)[1:5,],3)
    ordsortmat = matrix('', 5, dim(mu_matrix)[1])
    for (j in 1:5) {
      for (i in 1:dim(mu_matrix)[1]) {
        if (sortmat[j,i] == 0) {
          ordsortmat[j,i] = ''
        } else {
          ordsortmat[j,i] = paste(ordermat[j,i], '(', sortmat[j,i], ')', sep = '')
        }
      }
    }
    ordsortdf = as.data.frame(t(ordsortmat))
    ordsortdf = cbind(1:dim(mu_matrix)[1], ordsortdf)
    colnames(ordsortdf) = c('ID', 'Rank1(mu)', 'Rank2(mu)', 'Rank3(mu)', 'Rank4(mu)', 'Rank5(mu)')
    ordsortdf
  }, options = list(pageLength = 10))

  ########## plot7 : Influenced Rank ##########
  output$myplot7 = renderDataTable({
    mu_matrix.file = input$file3
    ext <- tools::file_ext(mu_matrix.file$datapath)
    req(mu_matrix.file)
    validate(need(ext == "csv", "Please upload a csv file"))
    mu_matrix = as.matrix(read.csv(mu_matrix.file$datapath))

    ordermat = apply(mu_matrix, 2, order, decreasing = TRUE)[1:5,]
    sortmat = round(apply(mu_matrix, 2, sort, decreasing = TRUE)[1:5,],3)
    ordsortmat = matrix('', 5, dim(mu_matrix)[1])
    for (j in 1:5) {
      for (i in 1:dim(mu_matrix)[1]) {
        if (sortmat[j,i] == 0) {
          ordsortmat[j,i] = ''
        } else {
          ordsortmat[j,i] = paste(ordermat[j,i], '(', sortmat[j,i], ')', sep = '')
        }
      }
    }
    ordsortdf = as.data.frame(t(ordsortmat))
    ordsortdf = cbind(1:dim(mu_matrix)[1], ordsortdf)
    colnames(ordsortdf) = c('ID', 'Rank1(mu)', 'Rank2(mu)', 'Rank3(mu)', 'Rank4(mu)', 'Rank5(mu)')
    ordsortdf
  }, options = list(pageLength = 10))
}

###############################################################################
shinyApp(ui = ui, server = server)

