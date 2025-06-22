# This will give us more detailed CPU information
check_cpu_config <- function() {
  # Get physical cores
  physical_cores <- system("nproc --all", intern = TRUE)
  
  # Get thread count per core (if hyperthreading is enabled)
  threads_per_core <- as.numeric(system("lscpu | grep 'Thread(s) per core' | awk '{print $4}'", intern = TRUE))
  
  # Get total available threads
  total_threads <- as.numeric(physical_cores) * threads_per_core
  
  return(list(
    physical_cores = physical_cores,
    threads_per_core = threads_per_core,
    total_threads = total_threads
  ))
}

print('Sourced: check_cpu_config()')

# Your original function modified to be more informative
get_cpu_info <- function() {
  pid <- Sys.getpid()
  cpu_affinity <- system(sprintf("taskset -cp %d", pid), intern = TRUE)
  
  # Get the number of cores this process can use
  available_cores <- length(strsplit(gsub(".*: ", "", cpu_affinity), ",")[[1]])
  
  return(list(
    pid = pid,
    cpu_affinity = cpu_affinity,
    process_threads = system(sprintf("ps -p %d -o nlwp=", pid), intern = TRUE),
    available_cores = available_cores
  ))
}

print('Sourced: get_cpu_info()')
