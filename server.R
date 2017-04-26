if(!require(shiny)){install.packages("shiny", repos="http://cran.us.r-project.org")}
if(!require(scales)){install.packages("scales", repos="http://cran.us.r-project.org")}
if(!require(ggplot2)){install.packages("ggplot2", repos="http://cran.us.r-project.org")}
if(!require(ggvis)){install.packages("ggvis", repos="http://cran.us.r-project.org")}
if(!require(magrittr)){install.packages("magrittr", repos="http://cran.us.r-project.org")}
if(!require(reshape2)){install.packages("reshape2", repos="http://cran.us.r-project.org")}
if(!require(data.table)){install.packages("data.table", repos="http://cran.us.r-project.org")}
if(!require(tidyr)){install.packages("tidyr", repos="http://cran.us.r-project.org")}
if(!require(dplyr)){install.packages("dplyr", repos="http://cran.us.r-project.org")}

library(shiny)
library(scales)
library(ggplot2)
library(ggvis)
library(magrittr)
library(reshape2)
library(data.table)
library(tidyr)
library(dplyr)


THEME <- theme(
	panel.background = element_rect(fill = 'white'),
	panel.grid.major = element_line(color = 'grey80', size = 0.25),
	panel.grid.minor = element_blank(),

	plot.title = element_text(size = 18, margin = margin(b = 15),
							  family = 'Helvetica',
							  face = 'bold',
							  hjust = 0.5),

	plot.subtitle = element_text(size = 16, margin = margin(b = 30),
								 family = 'Helvetica',
								 hjust = 0.5),

	axis.title.x = element_text(size = 14, vjust = -5),

	axis.title.y = element_text(size = 14, vjust = 10),

	axis.text.x = element_text(size = 12, margin = margin(t = 10)),
	axis.text.y = element_text(size = 12, margin = margin(r = 10)),

	axis.line = element_line(size = 1, color = 'grey40'),

	legend.background = element_rect(fill = "white", color = "grey"),
	legend.margin = margin(0.5, 1, 0.5, 1, 'cm'),
	legend.position = c(0.91, 0.8),
	legend.title = element_blank(),
	legend.text = element_text(size = 14),
	legend.key = element_rect(fill = "white", colour = NULL),
	legend.key.width = unit(1, 'line'),

	plot.margin = unit(c(1, 1, 1, 1), 'cm')
)

server <- function(input, output, session) {

	## GET THE DATA
	#setwd('~/documents/study/MSAN/coursework/spring/module-2/MSAN 622/hw3/')
	fb <- as.data.table(read.csv('data/dataset_Facebook.csv', sep = ";"))
	db <- as.data.table(read.csv('data/diabetic_data.csv'))

	bubble_tooltip <- function(x) {
		if(is.null(x)) return(NULL)

		labels <- gsub('([a-z])([A-Z])', '\\1 \\2', names(x)[!grepl('Type', names(x))])
		content <- x[!grepl('Type', names(x))]

		paste0("<div style='font-family: Helvetica Neue;
		   				font-size: 12px;'>",

			   "<span style='font-size: 16px;'><b>",
			   x[grepl('Type', names(x))],
			   "</b></span>",
			   "<br/>",
			   "<br/>",

			   paste0(
			   	"<b>", labels, ":</b> ",
			   	content,
			   	collapse = "<br />"
			   ),
			   "</div>"
		)
	}

	### FACEBOOK BUBBLE PLOT
	getFBData <- reactive({
		colnames(fb) <- gsub("\\.([a-zA-Z])", "\\U\\1", colnames(fb), perl = T)
		fb_data <- fb
		fb_data$PostWeekday <- c('MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT', 'SUN')[fb_data$PostWeekday]
		fb_data$PostMonth <- month.name[fb_data$PostMonth]

		fb_data$PostWeekday <- factor(fb_data$PostWeekday,
									  levels = c('MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT', 'SUN'))

		fb_data$PostHour <- factor(fb_data$PostHour,
								   levels = sort(1:24, dec = F))

		fb_data$Type <- factor(fb_data$Type,
							   levels = c('Status', 'Link', 'Photo', 'Video'))
		return(fb_data)
	})

	## CREATE THE PLOT
	getFBData %>%
		ggvis(x = ~LifetimePostTotalImpressions, y = ~LifetimePostConsumers) %>%
		set_options(height = 768, width = 1566, duration = 0) %>%
		layer_points(fill = ~Type, size = ~TotalInteractions, opacity := 0.9) %>%

		add_axis("x",
				 title = "Total Number of Unique Impressions",
				 title_offset = 50,
				 properties = axis_props(
				 	axis = list(stroke = "black", strokeWidth = 2),
				 	grid = list(stroke = "#F8F8F8"),
				 	ticks = list(stroke = "grey90", strokeWidth = 1),
				 	title = list(fontSize = 16),
				 	labels = list(angle = 0, align = "center", fontSize = 12)
				 )) %>%

		add_axis("y",
				 title = "Total Number of Consumers",
				 title_offset = 75,
				 properties = axis_props(
				 	axis = list(stroke = "black", strokeWidth = 2),
				 	grid = list(stroke = "#F8F8F8"),
				 	ticks = list(stroke = "grey90", strokeWidth = 1),
				 	title = list(fontSize = 16),
				 	labels = list(angle = 0, align = "right", fontSize = 12)
				 )) %>%

		hide_legend('size') %>%
		hide_legend('fill') %>%

		scale_numeric('size',
					  range = c(5, 1500)) %>%

		add_legend("fill",
				   title = "Post Type",
				   properties = legend_props(
				   	title = list(fontSize = 16, dy = -10),
				   	labels = list(fontSize = 14, dx = 5,
				   				  font = 'Helvetica Neue'),
				   	legend = list(y = 200)
				   )) %>%

		add_tooltip(bubble_tooltip, "hover") %>%
		bind_shiny('fbBubblePlot')


	### FB HEATMAP
	heatmap_tooltip <- function(x) {
		if(is.null(x)) return(NULL)

		time <- x[grepl('PostHour', names(x))]
		day <- x[grepl('PostWeekday', names(x))]
		content <- x[grepl('TotalInteractions', names(x))]

		paste0("<div style='font-family: Helvetica Neue;
		   				font-size: 12px;'>",

			   paste0(
			   	"<b>Total interactions on ",
			   	day, " at hour ", tolower(time), ":</b> ",
			   	content
			   ),
			   "</div>"
		)
	}

	getFBData %>%
		ggvis(x = ~PostHour, y = ~PostWeekday) %>%
		set_options(height = 468, width = 1566) %>%
		layer_rects(width = band(), height = band(),
					stroke := '#222222',
					strokeWidth := 0,
					fill = ~TotalInteractions) %>%

		add_axis("x",
				 title = "",
				 orient = 'top',
				 properties = axis_props(
				 	axis = list(stroke = "black", strokeWidth = 0),
				 	grid = list(strokeWidth = 0),
				 	ticks = list(stroke = "grey90", strokeWidth = 0),
				 	title = list(fontSize = 16),
				 	labels = list(angle = 0, align = "center", dy = 0,
				 				  font = 'Helvetica Neue',
				 				  fontSize = 16)
				 )) %>%

		add_axis("y",
				 title = "",
				 tick_padding = 10,
				 properties = axis_props(
				 	axis = list(stroke = "black", strokeWidth = 0),
				 	grid = list(strokeWidth = 0),
				 	ticks = list(stroke = "grey90", strokeWidth = 0),
				 	title = list(fontSize = 16),
				 	labels = list(angle = 0, align = "right", dx = 0,
				 				  font = 'Helvetica Neue',
				 				  fontSize = 14)
				 )) %>%

		scale_nominal('x', padding = 0.02) %>%
		scale_nominal('y', padding = 0.02) %>%

		scale_numeric('fill',
					  domain = c(0, 4000),
					  range = c('#3e6069', '#9ceaff'),
					  nice = T) %>%

	hide_legend('fill') %>%
	add_tooltip(heatmap_tooltip, "hover") %>%
	bind_shiny('fbHeatMap')


	## SMALL MULTIPLES PLOT
	output$smRaceSel <- renderUI({

		choices <- toupper(c('', 'African\nAmerican', 'Asian', 'Caucasian', 'Hispanic', 'Other'))
		names(choices) <- paste0(c('All Race', 'African American', 'Asian', 'Caucasian', 'Hispanic', 'Other'), 's')

		selectizeInput(inputId = 'smRaceSel',
					   label = 'Limit to',
					   choices = choices,
					   selected = NULL,
					   multiple = T)
	})

	output$smAgeBound <- renderUI({
		sliderInput(inputId = 'smAgeBound',
					label = 'over',
					value = 30,
					min = 0, max = 90,
					step = 10, round = T)
	})

	output$smSubmit <- renderUI({
		actionButton(inputId = 'smSubmit',
					 label = 'Show!')
	})

	getSMData <- eventReactive(input$smSubmit, {
		db_data <- db
		db_sm <- db_data[race != '?', ]
		db_sm <- db_sm[gender != 'Unknown/Invalid', ]
		db_sm$race <- toupper(gsub('([a-z])([A-Z])', '\\1\n\\2', db_sm$race))

		db_sm$age <- factor(db_sm$age,
							levels = paste0('[', paste(0:9*10, 1:10*10, sep="-"), ')'),
							labels = c('0-10 yrs', paste(paste(1:9*10 + 1, 2:10*10, sep="-"), 'yrs')))

		if (!is.null(input$smRaceSel)) {
			if (input$smRaceSel != '') {
				db_sm <- subset(db_sm, race %in% input$smRaceSel)
			}
		}
		if (!is.null(input$smAgeBound)) {
			db_sm <- subset(db_sm, as.numeric(age) > input$smAgeBound / 10)
		}

		return(db_sm)
	})

	output$smPlot <- renderPlot({
		plotdata <- getSMData()

		SM_THEME <- theme(
			panel.background = element_rect(fill = 'white')
			, panel.spacing = unit(1, 'lines')

			, plot.title = element_text(hjust = 0.5, size = 18, face = 'bold',
										margin = margin(t = 20))

			, axis.ticks = element_blank()
			, axis.title.x = element_text(size = 16, family = 'Helvetica Neue LT Std',
										  margin = margin(t = 20))

			, axis.title.y = element_text(size = 16, family = 'Helvetica Neue LT Std')

			, strip.background = element_blank()
			, strip.text.x = element_text(size = 14, margin = margin(t = 20, b = 20))
			, strip.text.y = element_text(size = 14, angle = 180,
										  margin = margin(l = 10, r = 50))


			, legend.position = 'top'
			, legend.title = element_text(face = 'bold', margin = margin(b = 10))
			, legend.text = element_text(size = 12, margin = margin(t = 10, b = 10))
		)

		p <- ggplot(data = plotdata,
					aes(x = time_in_hospital)) +

			facet_grid(race~age, switch = 'y') +
			geom_density(aes(fill = gender, color = gender),
						 size = 0.5,
						 alpha = 0.6,
						 stat = 'density') +

			xlab('Days Spent in Hospital') +
			ylab('Proportion of Patients\n') +

			scale_x_continuous(breaks = 1:5*5,
							   expand = c(0, 0)) +

			scale_y_continuous(breaks = c(0, 1),
							   position = 'right') +

			ggtitle('Hospital Wait Times for Diabetic Patients') +

			guides(fill = F) +
			scale_color_discrete(name = 'GENDER ') +
			SM_THEME

		return(p)
	})

	## PARALLEL COORDINATES PLOT
	preparePCData <- reactive({
		db_pc <- db

		### CLEANING
		db_pc <- db_pc[race != '?', ]
		db_pc <- db_pc[gender != 'Unknown/Invalid', ]
		db_pc$race <- gsub('([a-z])([A-Z])', '\\1 \\2', db_pc$race)
		db_pc$race <- factor(db_pc$race, levels = sort(unique(db_pc$race)))

		db_pc[, expired := ifelse(db_pc$discharge_disposition_id %in% c(20, 40, 41, 42), 1L, 0L)]
		db_pc[, discharge_disposition_id := NULL]

		db_pc$age <- factor(db_pc$age,
							levels = paste0('[', paste(0:9*10, 1:10*10, sep="-"), ')'),
							labels = c('0-10 yrs', paste(paste(1:9*10 + 1, 2:10*10, sep="-"), 'yrs')))

		### CLEANING
		colsToKeep <- c('patient_nbr', 'race', 'age', 'expired',
						names(db_pc)[grepl('num', names(db_pc)) & !grepl('(outpatient|lab)', names(db_pc))])

		db_pc_raw <- copy(db_pc)
		db_pc_raw <- db_pc_raw[, colsToKeep, with = F]

		db_pc_raw[, patient_nbr := unique(patient_nbr), by = 'patient_nbr']
		db_pc_raw[, race := max(as.integer(race)), by = 'patient_nbr']
		db_pc_raw[, age := min(as.integer(age)), by = 'patient_nbr']
		db_pc_raw[, expired := max(expired), by = 'patient_nbr']
		db_pc_raw[, num_procedures := sum(num_procedures), by = 'patient_nbr']
		db_pc_raw[, num_medications := sum(num_medications), by = 'patient_nbr']
		db_pc_raw[, number_emergency := sum(number_emergency), by = 'patient_nbr']
		db_pc_raw[, number_inpatient := sum(number_inpatient), by = 'patient_nbr']
		db_pc_raw[, number_diagnoses := sum(number_diagnoses), by = 'patient_nbr']

		db_pc_raw <- unique(db_pc_raw)
		db_pc_raw$patient_nbr <- as.factor(db_pc_raw$patient_nbr)
		db_pc_raw$age <- as.integer(db_pc_raw$age)

		## get an even sample of everyone
		db_pc_sample <- db_pc_raw[c(sample(which(db_pc_raw$expired == 1), 2),
									sample(which(db_pc_raw$race == 'Caucasian'), 100),
									sample(which(db_pc_raw$race == 'African American'), 100),
									sample(which(db_pc_raw$race == 'Asian'), 100),
									sample(which(db_pc_raw$race == 'Hispanic'), 100),
									sample(which(db_pc_raw$race == 'Other'), 100))
									,
									]

		## make everything go from 0 to 1
		db_pc_data <- copy(db_pc_sample)
		db_pc_data[, c(3, 5:ncol(db_pc_data)) := lapply(.SD[, c(-1, -2, -4)],
														function(col) col / max(col))]

		plot_data <- melt(db_pc_data, id.vars = c('patient_nbr', 'race'))[order(patient_nbr), ]
		plot_data$patient_nbr <- droplevels(plot_data$patient_nbr)

		plot_data$variable <- gsub('num(ber)*', '#', plot_data$variable)
		plot_data$variable <- gsub('_(.)', ' \\U\\1', plot_data$variable, perl = T)
		plot_data$variable <- gsub('^([a-z])', '\\U\\1', plot_data$variable, perl = T)

		## REORDER THE FACTORS
		plot_data$variable <- factor(plot_data$variable,
										  levels = c('Age',
										  		   	 '# Inpatient',
										  		     '# Diagnoses',
										  		   	 '# Medications',
										  		     '# Procedures',
										  		   	 '# Emergency',
										  		   	 'Expired'))

		plot_data <- plot_data

		## so we can set the labels
		age_labels <- c('', paste0('[', paste(0:9*10 + 1, 1:10*10, sep="-"), ')'))
		np_labels <- c(as.character(min(db_pc_sample$num_procedures)),
					   rep('', 3),
					   as.character(max(db_pc_sample$num_procedures)))
		nm_labels <- c(as.character(min(db_pc_sample$num_medications)),
					   rep('', 3),
					   as.character(max(db_pc_sample$num_medications)))
		ne_labels <- c(as.character(min(db_pc_sample$number_emergency)),
					   rep('', 3),
					   as.character(max(db_pc_sample$number_emergency)))
		ni_labels <- c(as.character(min(db_pc_sample$number_inpatient)),
					   rep('', 3),
					   as.character(max(db_pc_sample$number_inpatient)))
		nd_labels <- c(as.character(min(db_pc_sample$number_diagnoses)),
					   rep('', 3),
					   as.character(max(db_pc_sample$number_diagnoses)))

		ex_labels <- c('No', 'Yes')

		label_data <-
		rbindlist(
			list(
				data.frame(x = rep('Age', 11),
						   y = 0:10/10,
						   label = age_labels,
						   stringsAsFactors = F),

				data.frame(x = rep('# Procedures', 5),
						   y = 0:4/4,
						   label = np_labels,
						   stringsAsFactors = F),

				data.frame(x = rep('# Medications', 5),
						   y = 0:4/4,
						   label = nm_labels,
						   stringsAsFactors = F),

				data.frame(x = rep('# Emergency', 5),
						   y = 0:4/4,
						   label = ne_labels,
						   stringsAsFactors = F),

				data.frame(x = rep('# Inpatient', 5),
						   y = 0:4/4,
						   label = ni_labels,
						   stringsAsFactors = F),

				data.frame(x = rep('# Diagnoses', 5),
						   y = 0:4/4,
						   label = nd_labels,
						   stringsAsFactors = F),

				data.frame(x = rep('Expired', 2),
						   y = 0:1/1,
						   label = ex_labels,
						   stringsAsFactors = F)
			)
		)
		return(list(plot_data, label_data))
	})

	### MAKE THE PC PLOT
	reactive({
		data <- preparePCData()
		plot_data <- data[[1]]
		label_data <- data[[2]]

		lb <- linked_brush(keys = 1:nrow(plot_data), "dodgerblue")

		plot_data %>%
			as.data.frame() %>%
			arrange(patient_nbr, race, variable) %>%
			ggvis(x = ~variable, y = ~value) %>%
			set_options(height = 768, width = 1566, duration = 0) %>%
			layer_points(fill := 'black', size := 0,
						 size.brush := 0) %>%

			hide_legend('fill') %>%

			## ADD THE TEXT AND SCALES FOR EACH COLUMN
			layer_text(data = label_data[label_data$x == 'Age', ],
					   x = ~x,
					   y = ~y,
					   baseline := 'middle',
					   align := 'right',
					   dx := -10,
					   fontSize := 14,
					   text := ~label) %>%

			scale_nominal(property = 'text',
						  name = 'Age',
						  range = label_data[label_data$x == 'Age', ]$label) %>%

			layer_text(data = label_data[label_data$x == '# Procedures', ],
					   x = ~x,
					   y = ~y,
					   baseline := 'middle',
					   fontWeight := 'bold',
					   align := 'right',
					   dx := -10,
					   fontSize := 10,
					   text := ~label) %>%

			scale_nominal(property = 'text',
						  name = '# Procedures',
						  range = label_data[label_data$x == '# Procedures', ]$label) %>%

			layer_text(data = label_data[label_data$x == '# Medications', ],
					   x = ~x,
					   y = ~y,
					   baseline := 'middle',
					   fontWeight := 'bold',
					   align := 'right',
					   dx := -10,
					   fontSize := 10,
					   text := ~label) %>%

			scale_nominal(property = 'text',
						  name = '# Medications',
						  range = label_data[label_data$x == '# Medications', ]$label) %>%

			layer_text(data = label_data[label_data$x == '# Emergency', ],
					   x = ~x,
					   y = ~y,
					   baseline := 'middle',
					   fontWeight := 'bold',
					   align := 'right',
					   dx := -10,
					   fontSize := 10,
					   text := ~label) %>%

			scale_nominal(property = 'text',
						  name = '# Emergency',
						  range = label_data[label_data$x == '# Emergency', ]$label) %>%

			layer_text(data = label_data[label_data$x == '# Inpatient', ],
					   x = ~x,
					   y = ~y,
					   baseline := 'middle',
					   fontWeight := 'bold',
					   align := 'right',
					   dx := -10,
					   fontSize := 10,
					   text := ~label) %>%

			scale_nominal(property = 'text',
						  name = '# Inpatient',
						  range = label_data[label_data$x == '# Inpatient', ]$label) %>%

			layer_text(data = label_data[label_data$x == '# Diagnoses', ],
					   x = ~x,
					   y = ~y,
					   baseline := 'middle',
					   fontWeight := 'bold',
					   align := 'right',
					   dx := -10,
					   fontSize := 10,
					   text := ~label) %>%

			scale_nominal(property = 'text',
						  name = '# Diagnoses',
						  range = label_data[label_data$x == '# Diagnoses', ]$label) %>%

			layer_text(data = label_data[label_data$x == 'Expired', ],
					   x = ~x,
					   y = ~y,
					   baseline := 'middle',
					   fontWeight := 'bold',
					   font := 'Helvetica Neue',
					   align := 'left',
					   dx := 20,
					   fontSize := 14,
					   text := ~label) %>%

			scale_nominal(property = 'text',
						  name = 'Expired',
						  range = label_data[label_data$x == 'Expired', ]$label) %>%

			add_axis("x",
					 title = "",
					 orient = 'top',
					 properties = axis_props(
					 	axis = list(stroke = "black", strokeWidth = 0),
					 	grid = list(strokeWidth = 3, stroke = '#666666'),
					 	ticks = list(stroke = "grey90", strokeWidth = 0),
					 	title = list(fontSize = 16),
					 	labels = list(angle = 0, align = "center",
					 				  fontSize = 14, fontStyle = 'bold')
					 )) %>%

			add_axis("y",
					 title = "",
					 properties = axis_props(
					 	axis = list(stroke = "black", strokeWidth = 0),
					 	grid = list(strokeWidth = 0),
					 	ticks = list(stroke = "grey90", strokeWidth = 0),
					 	labels = list(angle = 0, align = "right",
					 				  fontSize = 0)
					 )) %>%

			add_legend("stroke",
					   title = "Race",
					   values = c('African American',
						   		  'Asian',
						   		  'Caucasian',
						   		  'Hispanic',
						   		  'Other'),
					   properties = legend_props(
					   	title = list(fontSize = 16, dy = -5),
					   	labels = list(fontSize = 14, dx = 5,
					   				  font = 'Helvetica Neue'),
					   	legend = list(y = 300)
					   )) %>%

			## BASE LINES
			group_by(patient_nbr, race) %>%
			layer_lines(stroke.brush := 'dodgerblue',
						strokeWidth := 0.75,
						stroke = ~race,
						opacity := 0.6) %>%

			lb$input() %>%
			layer_paths(opacity := 1, stroke := '#191919', strokeWidth := 2.5,
						data = reactive({
							if (!sum(lb$selected())) {
								return(data.frame(patient_nbr = as.factor('0'),
												  race = as.factor('Other'),
												  variable = as.factor('Age'),
												  value = 0.1))
							}
							sel <- plot_data[lb$selected(), ]
							sel_patient_nbrs <- unique(sel$patient_nbr)
							sel_fixed <- bind_rows(
								lapply(1:length(sel_patient_nbrs),
									   function(i) {
										   	patient_data <- plot_data[patient_nbr == sel_patient_nbrs[i], ][order(variable)]
										   	if (!(i %% 2)) {
										   		return(patient_data)
										   	} else {
										   		return(patient_data[rev(1:nrow(patient_data)), ])
										   	}
									   })
							)
							if (nrow(sel_fixed) > 1) {
								return(sel_fixed %>% as.data.frame())
							}
							return(sel %>% as.data.frame())
						})
			)
	}) %>% bind_shiny('PCPlot')

}
