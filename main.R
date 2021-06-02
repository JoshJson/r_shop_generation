# CONSTANTS
COMPANY_NAME = 'паралакс' # company name. Used to establish connection with working directory which should have
# the same name as the name of the company

P_SUPPLY = 5000 # price of supply
P_SALE = 8000 # price of each sale
P_UTIL = 400 # price of utilization of each product

EXT_SUPPLY <-'in' # file extension for files containing information about supply dynamic
EXT_SALE <-'out' # file extension for files containing information about sales dynamic

# ==============================

# SERVICE FUNCTIONS
# > strsplits - splits string into vector by looking for multiple or one delimiters in `splits` parameter
# > split_path - splits file path into vector

split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))

strsplits <- function(x, splits){
  for (split in splits) {
    x <- unlist(strsplit(x, split))
  }
  return(x[!x == ""])
} 

# ==============================

# GENERATION FUNCTIONS
# > generate_plot_specific - generates one plot for two files generated as a result of 
# `generate.data` function. If only one file is being generated. The plot will remain
# blank, because its purpose is to show correlation between sales and supply dynamics
# > generate.data - generates one single file `in` or `out` on the basis of user input
# > generate.data.assets - service function for generate.data. It generates desired vectors
# of random variables, i.e. product quantities
# > generate.data.specific - generated several files with multiple products on the basis
# of user input. Specific, i.e. fully customizable.

generate_plot_specific <- function(static, not_very_nice = NaN, status, name) {
  # > static - a discrete vector of random values which can be related to IN or OUT values
  # > not_very_nice - a discrete vector which is opposite to static (IN - OUT / OUT - IN).
  # If only one IN or OUT file is being created, the plot will not be generated, since to
  # show correlation between supply and sales we need two vectors of variables, i.e. 
  # variables for IN and OUT
  # > status - checking of what type is our static vector: is it related to IN or OUT. This
  # is later being checked in the code where we add legend on the plot and need to swap
  # curves' names if needed
  # > name - name of the shop that was previously defined by user. Used in plot's title
  if (!is.nan(not_very_nice)) {
    print(not_very_nice)
    print(static)
    plot(
      x = static,
      col = 'brown1',
      type = 'l',
      fg = 'deepskyblue1',
      lwd = 1,
      lty = 1, 
      pch = 20,
      main = paste0('Supply and sales dynamic for ',name),
      xlab = 'Day of week',
      ylab = 'Quantity',
      ylim = c(min(c(static, not_very_nice)) * 0.5, max(c(static, not_very_nice)) * 2)
    )
    lines(
      x = not_very_nice,
      col = 'gold3',
      type = 'b',
      lwd = 1,
      lty = 1, 
      pch = 20
    )
    if (status == 'in') {
      legend("topright", legend=c('Supply' , 'Sales'),
             col=c('brown1', 'gold3'), lty=1:2, cex = 0.8,
             title="Definitions", text.font=3)
    } else {
      legend("topright", legend=c('Sales' , 'Supply'),
             col=c('brown1', 'gold3'), lty=1:2, cex = 0.8,
             title="Definitions", text.font=3)
    }
  } else {
    print('[ERROR] One of two vectors is empty')
  }
}

generate.data.assets <- function(type_of_data_found,
                                 vector_of_found_data,
                                 initial_ndays,
                                 min, max) {
  # > type_of_data_found - 'IN' or 'OUT' 
  # > vector_of_found_data - vector of random variables, i.e. quantites of products.
  # It is not blank if the program has found another file with the same name but with
  # different extension. This means, if we've found 'IN' file then we need to generate
  # variables for 'OUT' file which should be less than variables in vector_of_found_data and vice
  # versa
  # > initial_ndays - amount of days that user has previously defined. Used to create a vector
  # later in the code
  # > min - minimum variable defined by user previously. Used in generation.
  # > max - maximum variable defined by user previously. Used in generation.
  output <- c()
  if (type_of_data_found == EXT_SUPPLY) {
    for (i in 1:initial_ndays) {
      init_rand <- sample(min:max, 1)
      while (init_rand > vector_of_found_data[i]) init_rand <- sample(min:max, 1)
      output <- c(output, init_rand)
    }
    return(output)
  } else {
    for (i in 1:initial_ndays) {
      init_rand <- sample(min:max, 1)
      while (init_rand < vector_of_found_data[i]) init_rand <- sample(min:max, 1)
      output <- c(output, init_rand)
    }
    return(output)
  }
}
generate.data <- function(filename,
                          path,
                          type_of_file,
                          min_boundary,
                          max_boundary,
                          ndays,
                          wdrvector) {
  filenaming = paste0(filename, '.', type_of_file)
  path_to_new_generated_file = c()
  working_path <- rev(wdrvector)
  working_path[1] <- gsub('/', '', working_path[1])
  my_path <- rev(split_path(path))
  vector_of_random_vals = runif(ndays, min = min_boundary,
                                max = max_boundary)
  if ((my_path[1] == '/') || (my_path[1] == '.') || (my_path[1] == 'C:/') || (my_path[1] == '') || (my_path[1] == 'D:/')) {
    my_path <- my_path[2:length(my_path)]
  }
  absolute_path <- paste(c(setdiff(working_path, my_path), my_path), collapse="/")
  if (!dir.exists(file.path(absolute_path))) {
  
    print(paste('[GENERATE.DATA LOG] New directory has been created, because could not find the entered one:', absolute_path), quote = F)
    dir.create(absolute_path, recursive = T)
  } else {
    print('[GENERATE.DATA LOG] Directory already exists. Checking for files with existing name..', quote = F)
    list_of_files = list.files(path = absolute_path)
    for (file in list_of_files) {
      file_params <- c(file)
      params <- strsplit(file_params, ".", fixed = T)[[1]]
      current_filename <- params[1]
      current_filetype <- params[length(params)]
      if ((current_filename == filename) && (current_filetype != type_of_file)) {
        setwd(absolute_path)
        initial_data = c()
        if (current_filetype == EXT_SUPPLY) {
          d = read.table(file, 
                         sep="\t", 
                         col.names=c("Дни", "Поставки"), 
                         fill=FALSE, 
                         strip.white=TRUE)
          st = 'out'
          rown = nrow(d)
          for (i in 2:rown) initial_data <- c(initial_data, as.integer(d$Поставки[i]))
          initials = initial_data
        } else {
          d = read.table(file, 
                         sep="\t", 
                         col.names=c("Дни", "Продажи"), 
                         fill=FALSE, 
                         strip.white=TRUE)
          st = 'in'
          rown = nrow(d)
          for (i in 2:rown) initial_data <- c(initial_data, as.integer(d$Продажи[i]))
          initials = initial_data
        }
        if (rown-1 < ndays) {
          print('[GENERATE.DATA LOG] File with existing name has been found. Number of days in the file is less than the entered number.', quote = F)
          ndays <- rown-1
          vector_of_random_vals <- generate.data.assets(current_filetype,
                                                        initial_data,
                                                        rown-1,
                                                        min_boundary,
                                                        max_boundary)
          break
        } else {
          print('[GENERATE.DATA LOG] File with existing name has been found. Number of days in the file is more or equal to the entered number.', quote = F)
          vector_of_random_vals <- generate.data.assets(current_filetype,
                                                        initial_data,
                                                        ndays,
                                                        min_boundary,
                                                        max_boundary)
          break
        }
      }
    }
  }
  setwd(absolute_path)
  if (!file.exists(filenaming)) {
    row_wrapper <- rep(0, ndays)
    if (type_of_file == EXT_SUPPLY) {
      dataframe = data.frame("Дни" = row_wrapper, Поставки = row_wrapper)
      for(i in 1:ndays){
        dataframe$Дни[i] <- i
        dataframe$Поставки[i] <- round(vector_of_random_vals[i])
      }
    } else {
      dataframe = data.frame("Дни" = row_wrapper, Продажи = row_wrapper)
      for(i in 1:ndays){
        dataframe$Дни[i] <- i
        dataframe$Продажи[i] <- round(vector_of_random_vals[i])
      }
    }
    generate_plot_specific(vector_of_random_vals, initials, st, filename)
    write.table(x = dataframe,
                file = filenaming,
                sep = "\t",
                row.names = F, quote = F)   
    print(paste('[GENERATE.DATA LOG] File has been successfully created:', filenaming), quote = F)
  } else {
    print('[GENERATE.DATA LOG] File already exists.', quote = F)
  }
}
# generate.data.price <- function() {
  
# }
generate_plot <- function(finalized_profits, index) {
  # > finalized_profits - vector of max sales per every product in one shop
  number_of_products = length(finalized_profits)
  
  png(filename=paste0("shopdata_generated_", index, ".png"), height=295, width=300, 
      bg="white") # writing data into PNG file
  
  barplot(finalized_profits, main=paste0("Sales per shop N", index), ylab= "Total",
          beside=TRUE, col=rainbow(number_of_products))
  
  legend("topright", legend=1:number_of_products, cex=0.6, 
         bty="n", fill=rainbow(number_of_products))
  
  
  dev.off()
}

generate.price <- function(sup, sal, uti, vector_sup, vector_sale) {
  tr_vector = c()
  tc_vector = c()
  prof = c()
  for (i in 1:length(vector_sup)) {
    tr_vector = c(tr_vector, as.integer(vector_sale[i]) * as.integer(sal)) # PROFIT
    q_util = as.integer(vector_sup[i]) - as.integer(vector_sale[i])
    tc_vector = c(tc_vector, as.integer(vector_sup[i]) * as.integer(sup) + q_util * as.integer(uti)) # TOTAL COSTS
    prof = c(prof, tr_vector[i] - tc_vector[i]) 
  }
  return(list(list(tr_vector), list(tc_vector), list(prof)))
}

generate.data.specific <- function(
  payload_n, payloads_of_days, 
  payloads_in, payloads_out, payloads_data,
  welcome, wdrvector, index, path = NaN) {
  # WELCOME - FULL FILENAME
  # PATH - CUSTOMIZABLE PATH
  # PAYLOADS_OF_DAYS -  '1 IN, 1 OUT periods:', payloads[[1]][1], payloads[[2]][2]
  working_path <- rev(wdrvector)
  if (!is.nan(path)) {
    my_path <- rev(split_path(path))
    if ((my_path[1] == '/') || (my_path[1] == '.') || (my_path[1] == 'C:/') || (my_path[1] == '') || (my_path[1] == 'D:/')) {
      my_path <- my_path[2:length(my_path)]
    }
    absolute_path <- paste(c(setdiff(working_path, my_path), my_path), collapse="/")
  } else {
    absolute_path <- paste(as.character(working_path), collapse = '/') 
  }
  if (!dir.exists(file.path(absolute_path))) {
    print(paste('[GENERATE.DATA LOG] New directory has been created, because could not find the entered one:', absolute_path), quote = F)
    dir.create(absolute_path, recursive = T)
  } else {
    print('[GENERATE.DATA.SPECIFIC LOG] Directory already exists.', quote = F)
  }
  setwd(absolute_path)
  
  list_of_files = list.files(path = absolute_path) # LISTING ALL FILES IN DIRECTORY TO CHECK ON DUPLICATES
  params_static = strsplit(welcome, ".", fixed = T)
  filename = params_static[1]
  for (file in list_of_files) {
    file_params = c(file)
    params <- strsplit(file_params, ".", fixed = T)[[1]]
    current_filename <- params[1]
    current_filetype <- params[length(params)]
    if ((current_filename == filename) && (current_filetype == 'shopdata')) {
      print(paste('[GENERATE.DATA.SPECIFIC LOG] Found similar file to', filename, '- new variables are going to be placed in that file.'), quote = F)
      break      
    }
  }
  dataframe = data.frame()
  list_of_data_for_generation = c()
  for (i in 1:payload_n) { # ЦИКЛ ПО КАЖДОМУ ТОВАРУ
    current_supplyprice = payloads_data[[i]][1]
    current_saleprice = payloads_data[[i]][2]
    current_utilprice = payloads_data[[i]][3]
    payloads_of_randoms_in <- randoms_in[[i]]
    payloads_of_randoms_out <- randoms_out[[i]]
    tr_tc = generate.price(current_supplyprice, 
                           current_saleprice, current_utilprice,
                           payloads_of_randoms_in,
                           payloads_of_randoms_out)
    tr = tr_tc[[1]]
    tc = tr_tc[[2]] # TOTAL COSTS
    prof = tr_tc[[3]]
    column_index = 1
    res_payload_of_days <- seq(1:payloads_of_days[[i]][1])
    # print(paste(payloads_of_days[[1]][1],payloads_of_days[[1]][2])) OK
    assign(paste0('Supply_days',i), res_payload_of_days)
    assign(paste0('Amount_of_Supplied',i), payloads_of_randoms_in)
    assign(paste0('Sales_days',i), res_payload_of_days)
    assign(paste0('Ammount_of_sales',i), payloads_of_randoms_out)
    assign(paste0('TR_',i), tr)
    assign(paste0('TC_',i), tc)
    assign(paste0('PROF_',i), prof)
    # should add UTILS, 
    eval(parse(text = paste0('class.data <- data.frame(',
                             paste0('Supply_days',i),',',
                             paste0('Amount_of_Supplied',i),',',
                             paste0('Sales_days',i),',',
                             paste0('Ammount_of_sales',i),',',
                             paste0('TR_',i),',',
                             paste0('TC_',i),',',
                             paste0('PROF_',i),')')))
    names(class.data)[column_index+4] <- paste0('TR_',i) # magic?
    names(class.data)[column_index+5] <- paste0('TC_',i) # magic?
    names(class.data)[column_index+6] <- paste0('PROF_',i) # magic?
    if (length(dataframe) == 0) {
      dataframe <- rbind(dataframe, class.data)
    } else {
      list.ofdf <- list(dataframe, class.data)
      max.rows <- max(unlist(lapply(list.ofdf, nrow), use.names = F))
      list.ofdf <- lapply(list.ofdf, function(x) {
        na.count <- max.rows - nrow(x)
        if (na.count > 0L) {
          na.dm <- matrix("", na.count, ncol(x))
          colnames(na.dm) <- colnames(x)
          rbind(x, na.dm)
        } else {
          x
        }
      })
      do.call(cbind, list.ofdf)
      dataframe <- data.frame()
      for (i in 1:(length(list.ofdf)-1)) {
        if (length(dataframe) == 0) {
          dataframe <- cbind(list.ofdf[[i]], list.ofdf[[i + 1]])
        } else {
          dataframe <- cbind(dataframe, list.ofdf[[i + 1]])
        }
      }
    }
    list_of_data_for_generation <- c(list_of_data_for_generation, max(unlist(prof)))
  }
  generate_plot(list_of_data_for_generation, index)
  print(dataframe)
  write.csv2(dataframe, welcome, fileEncoding = 'Windows-1251', row.names = F)
  print(paste('[GENERATE.DATA.SPECIFIC LOG] Generation complete (file and plot in PNG format) for: ',welcome), quote = F)
}

# ==============================

# MAIN PROGRAM

{
  working_dir = getwd()
  splitted_dir = split_path(working_dir)
  wd_name = splitted_dir[1]
  if (wd_name == COMPANY_NAME){
    analysis_dir = paste(working_dir, 'анализ', sep='/')
  } else {
    working_dir = paste0('C:/Users/1/Documents/R_scripts/', COMPANY_NAME)
    splitted_dir = split_path(working_dir)
    analysis_dir = paste(working_dir, 'анализ', sep='/')
  }
  generation_status_specific <- readline(print('Do you want to issue a fully customisable generation (multiple files)? (Y/N): '))
  if (generation_status_specific != 'y') {
    generation_status <- readline(print('Do you want to issue a stadart generation (one file)? (Y/N): '))
    if (tolower(generation_status) == 'y') {
      dir_path <- as.character(readline(prompt = 'Input absolute / relative path to file\'s directory (if multiple folders, split them by "/"): '))
      # C:/Users/1/Documents/R_scripts/паралакс/тест ИЛИ R_scripts/паралакс/тест 
      
      file_name <- as.character(readline(prompt = 'Input name of the file: '))
      # test 
      
      file_type <- as.character(readline(prompt = 'Input file extension (in/out): '))
      # in
      
      min <- as.integer(readline(prompt = 'Minimum quantity of product for generation (integer): '))
      max <- as.integer(readline(prompt = 'Maximum quantity of product for generation (integer): '))
      days <- as.integer(readline(prompt = 'Number of days (integer): '))
      print('[LOGGING] Standart generation is being held according to input parameters.', quote = F)
      generate.data(file_name, dir_path, file_type, min, max, days, splitted_dir[2:length(splitted_dir)])
    }
  } else {
    # if full generation is being issued
    
    name_of_file = readline('Input general name for files: ') # FILENAME IS A CSV TABLE
    payloads_shops_n = as.integer(readline('Number of shops, i.e. files:'))
    for (j in 1:payloads_shops_n) {
      randoms_in = list()
      randoms_out = list()
      payloads = list()
      data_payloads = list()
      print(paste('[GENERATE.DATA.SPECIFIC] Generating shop No', j), quote = F)
      payload_n = as.integer(readline(paste0('Number of products for shop No',j,': ')))
      # payload_n - КОЛИЧЕСТВО ТОВАРОВ ПО ДВУМ МАГАЗИНАМ: IN и OUT
      min <- 10
      for (i in 1:payload_n) {
        max <- round(sample(12:100, 1))
        vector_of_random_vals_out = c()
        interval_in = as.integer(readline(prompt = paste('Payload in (number of days for supply)', i, '- '))) # КОЛИЧЕСТВО ДНЕЙ ДЛЯ ОПРЕДЕЛЕННОГО ТОВАРА В IN
        interval_out = as.integer(readline(prompt = paste('Payload out (number of days for sales)', i, '- '))) # КОЛИЧЕСТВО ДНЕЙ ДЛЯ ОПРЕДЕЛЕННОГО ТОВАРА В OUT
        sup_sale_u = strsplit(readline('Input prices of SUPPLY, SALE and UTIL for this product (divide prices by spacebar): '), "\\s+") # a list object. Access with [[1]]
        vector_of_random_vals_in = round(sample(min:max, interval_in, replace = T))
        if (interval_in >= interval_out) { 
          boundary = interval_out
          for (i in 1:boundary) {
            init_rand <- round(sample(min:max, 1, replace = T))
            while (init_rand > vector_of_random_vals_in[i]) init_rand <- round(sample(min:max, 1, replace = T))
            vector_of_random_vals_out <- c(vector_of_random_vals_out, round(init_rand))
          }
          spare_boundary = interval_in - interval_out
          for (i in 1:spare_boundary) {
            vector_of_random_vals_out <- c(vector_of_random_vals_out, 0)
          }
        } else {
          boundary = interval_in
          for (i in 1:boundary) {
            init_rand <- round(sample(min:max, 1, replace = T))
            rand <- round(sample(min:max, 1, replace = T))
            vector_of_random_vals_out <- c(vector_of_random_vals_out, round(init_rand))
          }
        }
        randoms_in <- append(randoms_in, list(vector_of_random_vals_in)) 
        randoms_out <- append(randoms_out, list(vector_of_random_vals_out))  
        payload_of_days = list(c(as.integer(interval_in), as.integer(interval_out)))
        payloads <- append(payloads, payload_of_days)
        data_payloads <- append(data_payloads, list(lapply(sup_sale_u[[1]], as.integer)))
      }
      generate.data.specific(payload_n = payload_n, payloads_of_days = payloads, 
                             payloads_in = randoms_in, payloads_out = randoms_out, payloads_data = data_payloads, 
                             welcome = paste0(name_of_file,'_',j,'.csv'), wdrvector = splitted_dir, index = j)
    }
  }
  setwd(analysis_dir)
  print('[LOGGING] Directory checked.', quote = F)
  vector_of_filenames = list.files()
  num_of_initial_rows = length(vector_of_filenames) / 2
  row_wrapper <- rep(0, num_of_initial_rows)
  this_table = data.frame("Магазины" = row_wrapper, sd = row_wrapper, Выручка = row_wrapper, Прибыль = row_wrapper, Реализация = row_wrapper, Списание = row_wrapper)
  
  # IMPORTANT NOTICE:
  # unfortunately, the project has been initially made as a part of Russian university exam-task.
  # This is why most values are in Russian. The translation for values will be selected as a comment
  # in the following format:
  # * value in Russian - translation
  
  # * Продажи_макс - max sales
  # * День - days for max sales
  # * Продажи_мин - min sales
  # * Списание_макс - max utilization
  # * День_ - days for min sales
  # * День__ - days for max utilization
  
  this_table$Продажи_макс <- row_wrapper
  this_table$День <- row_wrapper
  this_table$Продажи_мин <- row_wrapper
  this_table$День_ <- row_wrapper
  this_table$Списание_макс <- row_wrapper
  this_table$День__ <- row_wrapper
  print('[LOGGING] Dataframe for shop analysis has been created.', quote = F)
  for(i in 1:num_of_initial_rows) {
    j = 2 * i - 1
    name_of_in = paste('in', i, sep='_')
    name_of_out = paste('out', i, sep='_')
    assign(name_of_in, read.table(vector_of_filenames[j], header = T, encoding = 'UTF-8'))
    assign(name_of_out, read.table(vector_of_filenames[j+1], header = T, encoding = 'UTF-8'))
    name_of_in = eval(parse(text = name_of_in))
    name_of_out = eval(parse(text = name_of_out))
    num_of_rows = nrow(name_of_in)
    this_table$Магазины[i] <- paste0('Shop №', i)
    vector_for_quadratic = c()
    vector_for_util = c()
    for (k in 1:num_of_rows) {
      q_sale = name_of_out[k, 2]
      q_supply = name_of_in[k, 2]
      q_util = q_supply - q_sale
      vector_for_quadratic = c(vector_for_quadratic, q_sale)
      vector_for_util = c(vector_for_util, q_util)
      tr = P_SALE * q_sale # profit
      tc = q_supply * P_SUPPLY + q_util * P_UTIL # total costs
      
      # * Выручка - profit
      # * Прибыль - finalized profit with costs
      # * Издержки - total costs
      # * Реализация - realization (sales)
      # * Списание - total utilization
      
      this_table$Выручка[i] <- tr
      this_table$Прибыль[i] <- tr-tc
      this_table$Издержки[i] <- tc
      this_table$Реализация[i] <- q_sale
      this_table$Списание[i] <- q_util
    }
    max_el = max(vector_for_quadratic) # max element
    min_el = min(vector_for_quadratic) # min elemet
    max_util = max(vector_for_util)
    index_of_max = which.max(vector_for_quadratic) # index of max sales
    index_of_min = which.min(vector_for_quadratic) # index of min sales
    index_of_max_util = which.max(vector_for_util)
    this_table[i, 8] <- index_of_max # day of max sales
    this_table[i, 10] <- index_of_min # day of min sales
    this_table[i, 12] <- index_of_max_util # day of max util
    this_table$Продажи_макс[i] <- max_el
    this_table$Продажи_мин[i] <- min_el
    this_table$Списание_макс[i] <- max_util
    this_table$sd[i] = round(sd(vector_for_quadratic), 2)
  }
  print('[LOGGING] Main calculations have been finished.', quote = F)
  path_to_table = paste(working_dir, 'final_result.csv', sep = '/')
  if (!file.exists(path_to_table)) {
    write.csv2(this_table, path_to_table, fileEncoding = 'Windows-1251', row.names = F)
    print('[LOGGING] Data has been successfully generated and imported into CSV table. Opening generated table..', quote = F)
    shell(path_to_table)
  } else {
    print('[ERROR] The CSV file with data has been already generated.', quote = F)
  }
}