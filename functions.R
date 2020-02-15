key <- Sys.getenv('GOOGLEWAY_GOOGLEMAPS')
set_key(key)

# geocode the address entered into the serach box and get the lat / long
geocode_address <- function(location_term) {
  
  # geocode the address entered as the search term and extract lat / long
  map_center <- google_geocode(location_term)$results$geometry$location
  
  # convert to vector (lat then long), which is the format needed for both leaflet and the google maps API
  map_center <- c(map_center[[1]], map_center[[2]])
  
  return(map_center)
  
}

# retrieve place information from google maps
get_google_places <- function(map_center, search_term) {
  

  # initialize dataframe to store all search results
  df <- data.frame()
  
  # repeat loop as long as there is a page_token value in the returned search
  # a page_token value signifies that there are results on an additional page
  repeat {
    
    # conduct search of google maps
    place_search <- google_places(location = map_center,
                                  keyword = search_term,
                                  radius = 20000, # search distance from location in meters
                                  # there will be no place_token variable for first iteration,
                                  # so make value NULL
                                  # otherwise, use the place_token variable
                                  page_token = if (exists('place_token')) place_token else NULL)
    
    # create a dataframe out of the needed information in the search results
    df <- bind_cols(
        id = place_search$results$place_id,
        name = place_search$results$name,
        location = place_search$results$geometry$location,
        address = place_search$results$vicinity,
        search = rep(search_term, length(place_search$results$name))
      ) %>%
      bind_rows(df, .)
    
    place_token <- place_search$next_page_token
    
    # wait 1 second before calling API again,
    # needed so we don't call API too fast and cause error
    Sys.sleep(1)
    
    # when places_token is null, there are no more search listing
    # therefore, berak out of loop
    if (is.null(place_token)) {break}
  }
  
  # add phone number, website, and address to dataset
  df <- add_web_phone_address(df) %>%
    select(-id) %>%
    # remove the word 'County' from the county name
    mutate(county = str_replace(county, " County", "")) %>%
    # reorder variable
    select(name, search, lat:address, city:zip, phone, web)
  
  # need to return the dataframe of locations and address lat / long
  # the address lat / long will be used to center the leaflet map
  return(df = df)
  
}

# add website, phone number, and address to dataset of places
# this function is not called in the app, it is used in another function
add_web_phone_address <- function(df) {
  
  # pull place details from google maps api
  details <- map(df$id, google_place_details)
  
  # pull phone numbers from each place
  phone_number <- details %>%
    map('result') %>%
    # if there is no phone number, make value 'None'
    map_chr('formatted_phone_number', .default = 'None')
  
  # pull website from each place
  web_address <- details %>%
    map('result') %>%
    # if there is no website, make value 'None'
    map_chr('website', .default = 'None')
  
  # pull address from each location
  # address comes as its own list of components that we must iterate through
  address <- details %>%
    # custom function that parses list of address information
    map(get_address) %>%
    bind_rows()
  
  # add phone number and website to dataset
  df <- df %>%
    mutate(phone = phone_number,
           web = web_address) %>%
    # address has multiple columns, so it must be bound to data frame
    bind_cols(address)
  
  return(df)
  
}

# address information comes as a list for town, county, zip, ect
# convert this information to where for each place, all address information
# is in a single row of a dataframe with multiple columns
get_address <- function(ind_place) {
  
  # some addresses have multiple values in a vector in a single row
  # for rows with vectors of multiple values, one of the values is always
  # "political", so we will remove this value
  addresses <- unlist(ind_place$result$address_components$types)
  addresses <- addresses[addresses != "political"]
  
  ind_place$result$address_components %>%
    mutate(types = !! addresses) %>%
    # only keep town, zip code, and county
    filter(types %in% c('locality', 'postal_code',
                        'administrative_area_level_2',
                        'administrative_area_level_1')) %>%
    # make address labels more descriptive
    mutate(types = recode(types,
                          locality = "city",
                          postal_code = "zip",
                          administrative_area_level_2 = "county",
                          administrative_area_level_1 = "state")) %>%
    select(-short_name) %>%
    # convert to wide form so that each geographic label of the address is 
    # on a different column of the dataframe
    pivot_wider(names_from = types, values_from = long_name)
}