library(shiny)
library(scales)
library(ggplot2)
library(ggvis)
library(magrittr)
library(reshape2)
library(data.table)
library(tidyr)
library(dplyr)


ui <- fluidPage(

	titlePanel(strong('HW3 Visualizations')),

	br(),
	br(),

	tabsetPanel(
		tabPanel('Facebook Bubble Plot',
			br(),
			br(),
			fluidRow(column(1), ggvisOutput('fbBubblePlot'), column(1))
		),

		tabPanel('Facebook Heatmap',
			br(),
			br(),
			br(),
			br(),
			br(),
			fluidRow(column(1), ggvisOutput('fbHeatMap'), column(1))
		),

		tabPanel('Comparing Race, Age, and Length of Hospital Stay for Diabetes',
			sidebarLayout(
				sidebarPanel(
					tags$style(" .well { background-color: white;
							   			 border: 0px;
							   			 webkit-box-shadow: none;
							   			 box-shadow: none; } "),

					uiOutput('smRaceSel')
					, uiOutput('smAgeBound')
					, br()
					, uiOutput('smSubmit')
					, width = 2
				),

				mainPanel(
					plotOutput('smPlot',
							   width = '1560px', height = '800px'),
					width = 10
				)
			)
		),

		tabPanel('Parallel Coordinate Plot of Diabetes Related Factors',
			br(),
			br(),
			fluidRow(column(1), ggvisOutput('PCPlot'), column(1))
		),
		type = 'pills'
	)
)
