not_all_na <- function(x) any(!is.na(x))


Fetch_land_optimizer <- function(deck_list_path, card_DB = df_base) {
  df_land <- card_DB %>%
    filter(str_detect(tolower(type_line), "land")) %>%
    mutate(
      across(c(type_line, name), ~ tolower(.))
    )

  test_deck_list <- deck_parser(deck_list_path) # deck_list_path)


  Fetch_land <- df_land %>%
    select(name, oracle_text, image_uris.large) %>%
    filter(str_detect(tolower(oracle_text), "^\\{t\\}, pay 1 life, sacrifice")) %>%
    mutate(fetchable_land = str_extract(
      tolower(oracle_text),
      "(?<=\\: search your library for .{1,2}\\s)(.+)(?=card, put it onto the battlefield, then shuffle\\.)"
    )) %>%
    separate_wider_delim(fetchable_land,
      " or ",
      names = c("A", "B"),
      too_few = c("align_start")
    ) %>%
    select(-oracle_text) %>%
    pivot_longer(-c(name, image_uris.large), names_to = "temp", values_to = "type_fetchable") %>%
    mutate(type_fetchable = trimws(type_fetchable, which = "both")) %>%
    select(-temp) %>%
    drop_na() %>%
    dplyr::rename(
      fetch_name = name,
      fetch_image = image_uris.large
    )



  df_of_fetchable_land <- df_land %>%
    fuzzyjoin::fuzzy_left_join(Fetch_land,
      by = c("type_line" = "type_fetchable"),
      match_fun = str_detect
    ) %>%
    select(
      name, type_line, fetch_name, type_fetchable # ,image_uris.large,fetch_image
    ) %>%
    filter(!is.na(fetch_name)) %>%
    distinct(name, fetch_name, .keep_all = TRUE)



  fetch_in_deck <- test_deck_list %>%
    filter(test_deck_list$nom %in% unique(Fetch_land$fetch_name))


  fetchable_land_in_deck <- right_join(
    df_of_fetchable_land,
    test_deck_list,
    by = c("name" = "nom")
  ) %>%
    drop_na() %>%
    # uncount(quantite) %>%
    select(-sep_side)

  number_of_fetch_in_deck <- sum(fetch_in_deck$quantite)
  vector_of_fetchable_unique_land <- unique(fetchable_land_in_deck$name)



  initial_table_of_fetch_opti <- fetchable_land_in_deck %>%
    group_by(fetch_name) %>%
    summarise(
      nb_fetchable = sum(quantite),
      land_fetch = list(name)
    ) %>%
    arrange(desc(nb_fetchable)) %>%
    rowwise() %>%
    mutate(
      not_fetch_land = list(vector_of_fetchable_unique_land[vector_of_fetchable_unique_land %notin% land_fetch])
      # not_fetch_land = land_fetch %notin% unique(fetchable_land_in_deck$name)
    )

  fetch_opti <- initial_table_of_fetch_opti %>%
    select(fetch_name, nb_fetchable)

  land_opti <- initial_table_of_fetch_opti %>%
    select(fetch_name, not_fetch_land) %>%
    unnest(not_fetch_land)

  fetch_opti <- initial_table_of_fetch_opti %>%
    filter(nb_fetchable >= unique(.$nb_fetchable)[3]) %>%
    select(fetch_name, nb_fetchable)

  based_data_of_fetch <- as.data.frame(matrix(rep(0:4, length(fetch_opti$fetch_name)), ncol = length(fetch_opti$fetch_name)))

  colnames(based_data_of_fetch) <- fetch_opti$fetch_name

  based_data_of_fetch_combin <- tidyr::expand(
    based_data_of_fetch,
    !!!based_data_of_fetch
  ) %>%
    filter(rowSums(.) == number_of_fetch_in_deck)



  Res_best_fetch_for_number_of_fetchable <- cbind(based_data_of_fetch_combin,
    total_number_fetchable = rowSums(
      sweep(
        as.matrix(based_data_of_fetch_combin),
        MARGIN = 2,
        fetch_opti$nb_fetchable, `*`
      )
    )
  ) %>%
    rownames_to_column()

  Min_fetchable_df <- initial_table_of_fetch_opti %>%
    filter(nb_fetchable >= unique(.$nb_fetchable)[3]) %>%
    select(-not_fetch_land, -nb_fetchable) %>%
    unnest(land_fetch) %>%
    left_join(
      fetchable_land_in_deck %>%
        select(name, quantite) %>%
        distinct(),
      by = c("land_fetch" = "name")
    ) %>%
    complete(fetch_name, land_fetch,
      fill = list(quantite = 0)
    ) %>%
    full_join(
      based_data_of_fetch_combin %>%
        rownames_to_column() %>%
        pivot_longer(-rowname),
      by = c("fetch_name" = "name")
    )


  result_total_fetchable <-
    inner_join(
      Min_fetchable_df %>%
        mutate(res = quantite * value) %>%
        group_by(rowname, land_fetch) %>%
        summarise(test = sum(res)) %>%
        summarise(min_number_of_fetchable = min(test)),
      Res_best_fetch_for_number_of_fetchable,
      by = "rowname"
    )





  opti_number_of_fetchable <- result_total_fetchable$rowname[
    result_total_fetchable$total_number_fetchable == max(result_total_fetchable$total_number_fetchable)
  ]


  opti_min_number_of_fetchable <- result_total_fetchable$rowname[
    result_total_fetchable$min_number_of_fetchable == max(result_total_fetchable$min_number_of_fetchable)
  ]


  common_solution_id <- intersect(opti_min_number_of_fetchable, opti_number_of_fetchable)



  if (length(common_solution_id) > 1) {
    res <- result_total_fetchable %>%
      filter(rowname %in% common_solution_id) %>%
      select(-rowname, -min_number_of_fetchable, -total_number_fetchable) %>%
      select(where(~ is.numeric(.x) && sum(.x) > 0))
    # print("real optimum found")
    # print(
    #   result_total_fetchable %>%
    #         filter(rowname %in% common_solution_id) %>%
    #         select(-rowname,-min_number_of_fetchable,-total_number_fetchable) %>%
    #         select(where( ~ is.numeric(.x) && sum(.x) > 0))
    #       )
  } else {
    res <- list(
      max_number_of_fetchable = result_total_fetchable %>%
        filter(rowname %in% opti_number_of_fetchable) %>%
        select(where(~ is.numeric(.x) && sum(.x) > 0)) %>%
        arrange(desc(total_number_fetchable), desc(min_number_of_fetchable)),
      max_the_min_number_of_fetchable = result_total_fetchable %>%
        filter(rowname %in% opti_min_number_of_fetchable) %>%
        select(where(~ is.numeric(.x) && sum(.x) > 0)) %>%
        arrange(desc(total_number_fetchable))
    )
    # print("No real optimum found")
    # print("Optimum that maximize number of fetchable lands" )
    # print(result_total_fetchable %>%
    #         filter(rowname %in% opti_number_of_fetchable) %>%
    #         select(where( ~ is.numeric(.x) && sum(.x) > 0)))

    # print("Optimum that maximize the minimum number of fetchable lands" )
    # print(
    #   result_total_fetchable %>%
    #         filter(rowname %in% opti_min_number_of_fetchable) %>%
    #         select(where( ~ is.numeric(.x) && sum(.x) > 0)) %>%
    #         arrange(desc(total_number_fetchable))
    #       )
  }


  return(res)
}







################################################################################
# short_function that find missing cards
find_missing_cards <- function(path_to_list,
                               scryfall_df,
                               collection_df) {
  deck_list_en_cours <- deck_parser(deck_path = path_to_list)

  
decklist_match_with_scryfall_name <- deck_list_en_cours %>%
  distinct() %>%
  group_by(nom) %>%
  summarise(quantite = sum(quantite)) %>%
  left_join(
    join_with_scryfall(
      Df_with_cardname =   .,
      cardname_col = "nom",
      scry_fall_df = scryfall_df
    ),
    by = c("nom" = "CardName")
  ) %>%
inner_join(
  scryfall_df %>%
    select(id, name),
  by = join_by(
    scry_fall_id == id
  )
) %>%
select(-scry_fall_id)



  
  final_res <- decklist_match_with_scryfall_name %>%
    left_join(
      collection_df,
      by = c("name" = "Name")
    ) %>%
    mutate(
      Quantity = if_else(is.na(Quantity), 0, Quantity),
      Missing_quant = quantite - Quantity
    ) %>%
    filter(Missing_quant > 0) %>%
    select(
      Missing_quant,
      nom
    )

  return(final_res)
}


################################################################################
# Sub_fun of organising_deckbox
search_reamining <- function(
    dek_list_fun,
    previous_iter_common = NULL,
    n = 0) {
  # print(n)
  # if(n > 10)  browser()
  if (is.null(previous_iter_common)) {
    Common_cards <- dek_list_fun %>%
      select(-sep_side) %>%
      group_by(nom, deck) %>%
      summarise(quantite = sum(quantite), .groups = "drop") %>%
      group_by(nom) %>%
      summarise(
        # a = list(quantite),
        min_common = min(quantite),
        deck = list(sort(deck))
      ) %>%
      rowwise() %>%
      mutate(
        n_common = length(deck)
      ) %>%
      filter(n_common > 1) %>%
      ungroup() %>%
      mutate(grouping_column = paste0(deck)) %>%
      unnest_longer(deck) %>%
      select(-n_common)

    reamaining_common <- data.frame()
  } else {
    reamaining_common <- dek_list_fun %>%
      select(-sep_side) %>%
      group_by(nom, deck) %>%
      summarise(quantite = sum(quantite), .groups = "drop") %>%
      left_join(
        previous_iter_common %>%
          group_by(nom, deck) %>%
          summarise(min_common = sum(min_common), .groups = "drop"),
        by = c("nom", "deck")
      ) %>%
      drop_na(min_common) %>%
      mutate(quantite = quantite - min_common) %>%
      select(-min_common) %>%
      filter(quantite > 0) %>%
      group_by(nom) %>%
      mutate(number_of_occurence = n()) %>%
      filter(number_of_occurence > 1) %>%
      select(-number_of_occurence) %>%
      group_by(nom, deck) %>%
      summarise(quantite = sum(quantite), .groups = "drop") %>%
      group_by(nom) %>%
      summarise(
        min_common = min(quantite),
        deck = list(sort(deck)),
        .groups = "drop"
      ) %>%
      rowwise() %>%
      mutate(
        n_common = length(deck)
      ) %>%
      filter(n_common > 1) %>%
      ungroup() %>%
      mutate(grouping_column = paste0(deck)) %>%
      unnest_longer(deck) %>%
      select(-n_common)


    Common_cards <- rbind(previous_iter_common, reamaining_common)
  }

  if (nrow(reamaining_common) > 0 | is.null(previous_iter_common)) {
    res <- search_reamining(dek_list_fun, Common_cards, n = n + 1)
  } else {
    res <- Common_cards
  }
  return(res)
}



################################################################################
# Organising deck box

organising_deckbox <- function(deck_list_base_path) {
  deck_to_compare_list <- list.files(deck_list_base_path, full.names = TRUE)

  Deck_list <- lapply(
    deck_to_compare_list, function(x) {
      res <- deck_parser(x)
      res$sep_side[which(res$sep_side)[1]:nrow(res)] <- TRUE

      res_final <- res %>%
        mutate(deck = str_remove(
          str_remove(x, paste0("^", deck_list_base_path)), "\\.txt$"
        )) %>%
        drop_na()
      return(res_final)
    }
  ) %>%
    bind_rows()

  # dek_list_fun <- Deck_list
  # previous_iter_common <- Common_cards
  # problème sur saisie push
  Common_cards <- search_reamining(Deck_list)

  common_list_of_cards_by_deck <- Common_cards %>%
    group_by(grouping_column, nom) %>%
    mutate(deck_using_cards = paste0(deck, collapse = " : ")) %>%
    ungroup() %>%
    select(-deck, -grouping_column) %>%
    group_by(deck_using_cards) %>%
    distinct() %>%
    group_split()
  # Group by a
  # group split df and print
  # remove cards from deck starting by side and print
  # reflechir a visualisations avec image des cartes (surement a globaliser avec une fonction)

  No_common_cards <- Deck_list %>%
    anti_join(Common_cards, by = "nom")

  Common_cards_remove_from_side <- Common_cards %>%
    unnest_longer(deck) %>%
    inner_join(Deck_list %>% filter(sep_side), by = c(
      "nom" = "nom",
      "deck" = "deck"
    )) %>%
    mutate(
      min_common2 = if_else(min_common <= quantite, 0, min_common - quantite),
      quantite = if_else(min_common <= quantite, quantite - min_common, 0)
    ) %>%
    select(-min_common) %>%
    rename(min_common = min_common2)

  Common_cards_remove_from_main <- Common_cards %>%
    # unnest_longer(deck) %>%
    left_join(
      Common_cards_remove_from_side %>%
        select(-sep_side, -quantite),
      by = c("nom", "deck", "grouping_column")
    ) %>%
    mutate(min_common = if_else(
      is.na(min_common.y), min_common.x, min_common.y
    )) %>%
    select(-min_common.x, -min_common.y) %>%
    group_by(nom, deck) %>%
    summarise(
      min_common = sum(min_common),
      .groups = "drop"
    ) %>%
    # filter(min_common > 0) %>%
    inner_join(Deck_list %>%
      filter(!sep_side), by = c(
      "nom" = "nom",
      "deck" = "deck"
    )) %>%
    mutate(quantite = quantite - min_common) %>%
    filter(quantite > 0) %>%
    select(
      -min_common # ,-grouping_column
    )

  Not_common_list_of_cards <- rbind(
    rbind(
      No_common_cards,
      Common_cards_remove_from_main
    ),
    Common_cards_remove_from_side %>%
      select(-min_common, -grouping_column) %>%
      filter(quantite > 0)
  ) %>%
    arrange(deck, sep_side) %>%
    group_split(deck)

  unlink("outpout/Deck_box.txt")
  write(paste0("Common cards : ", "\n"), file = "outpout/Deck_box.txt", append = TRUE)
  invisible(
    lapply(common_list_of_cards_by_deck, function(x) {
      # print(unique(x$deck_using_cards))
      write(paste0(unique(x$deck_using_cards)),
        file = "outpout/Deck_box.txt", append = TRUE
      )
      write_tsv(x %>% select(-deck_using_cards),
        file = "outpout/Deck_box.txt", append = TRUE
      )
      write("\n", file = "outpout/Deck_box.txt", append = TRUE)
      #
      # x %>%  print(n = 100)
    })
  )

  write(paste0("Separate Deck : ", "\n"), file = "outpout/Deck_box.txt", append = TRUE)
  invisible(lapply(Not_common_list_of_cards, function(x) {
    write(paste0(unique(x$deck)), file = "outpout/Deck_box.txt", append = TRUE)
    write(paste0("Maindeck :"), file = "outpout/Deck_box.txt", append = TRUE)
    write_tsv(x %>% filter(!sep_side) %>% select(-deck, -sep_side),
      file = "outpout/Deck_box.txt", append = TRUE
    )
    write(paste0("Sideboard :"), file = "outpout/Deck_box.txt", append = TRUE)
    write_tsv(x %>% filter(sep_side) %>% select(-deck, -sep_side),
      file = "outpout/Deck_box.txt", append = TRUE
    )
    write("\n", file = "outpout/Deck_box.txt", append = TRUE)
  }))

  return(
    list(
      common = common_list_of_cards_by_deck,
      not_common = Not_common_list_of_cards
    )
  )
}
################################################################################


###############################################################################
################### Pitch evaluation ###########################################

proba_compute <- function(success, hit, deck, sample_size, more_than = FALSE) {
  if (more_than) {
    sum(dhyper(x = success:hit, m = hit, n = deck - hit, k = sample_size))
  } else {
    dhyper(x = success, m = hit, n = deck - hit, k = sample_size)
  }
}



Pitch_cards_evaluation <- function(deck_list_path, card_DB) {
  pitch_cards <- rbind(
    list(
      w = c("Scars of the Veteran", "Reverent Mantra", "Shining Shoal", "Force of Virtue", "Solitude"),
      u = c("Force of Will", "Misdirection", "Disrupting Shoal", "Force of Negation", "Subtlety"),
      b = c("Contagion", "Unmask", "Sickening Shoal", "Force of Despair", "Grief"),
      r = c("Pyrokinesis", "Cave-In", "Blazing Shoal", "Force of Rage", "Fury"),
      g = c("Bounty of the Hunt", "Vine Dryad", "Nourishing Shoal", "Force of Vigor", "Endurance")
    ) %>%
      bind_rows() %>%
      pivot_longer(everything(), values_to = "cards", names_to = "color") %>%
      mutate(number_pitch = 1),
    list(
      w = c("Sunscour"),
      u = c("Commandeer"),
      b = c("Soul Spike"),
      r = c("Fury of the Horde"),
      g = c("Allosaurus Rider")
    ) %>%
      bind_rows() %>%
      pivot_longer(everything(), values_to = "cards", names_to = "color") %>%
      mutate(number_pitch = 2)
  ) %>%
    mutate(cards = tolower(cards))




  ###################################
  # pour le legacy donc plus tard

  pitch_discard_land <- list(
    white = c("Abolish"),
    blue = c("Foil"),
    black = c("Outbreak"),
    red = c("Flameshot"),
    green = c("Snag")
  )




  test_deck_list <- deck_parser(deck_list_path)




  identify_pitch_cards <- inner_join(pitch_cards, test_deck_list, by = c("cards" = "nom"))
  if (nrow(identify_pitch_cards) == 0) {
    print("no pitch_cards")

    list_of_result_table <- NULL
  } else {
    # a <- pitchable_cards %>% filter(name == "Purple Worm // Purple Worm")

    pitchable_cards <- card_DB %>%
      select(
        name,
        matches("^card_faces\\.colors\\.\\d+"), # card_faces.colors, #colors,
        matches("^colors\\.\\d+$")
      ) %>%
      filter(name != "") %>%
      select(where(not_all_na)) %>%
      mutate(
        colors0 = if_else(
          !is.na(card_faces.colors.1.1),
          card_faces.colors.1.1, colors.1
        ),
        .after = 1
      ) %>%
      mutate(name = tolower(name)) %>%
      select(
        -starts_with("card_faces") # ,
        #   -colors
      ) %>%
      pivot_longer(
        c(
          starts_with("colors")
          # , starts_with("card_faces")
        ),
        names_to = "colors"
      ) %>%
      drop_na(value) %>%
      select(-colors) %>%
      distinct() %>%
      mutate(val = 1) %>%
      pivot_wider(names_from = value, values_from = val, values_fill = 0) %>%
      mutate(across(-name, ~ . == 1)) %>%
      right_join(
        test_deck_list,
        by = c("name" = "nom")
      ) %>%
      drop_na(R)


    color_of_interest <- toupper(identify_pitch_cards[1, ]$color)

    number_of_pitachable <- cbind(pitchable_cards %>%
      # filter(!sep_side) %>%
      filter(!!rlang::sym(color_of_interest)) %>%
      group_by(sep_side) %>%
      summarise(sum = sum(quantite), .groups = "drop") %>%
      select(-sep_side), name = c("number_pitachabel", "max_add")) %>%
      pivot_wider(values_from = sum, names_from = name) %>%
      mutate(color = identify_pitch_cards[1, ]$color)



    pitch_cards_and_number_of_pitachable <- lapply(
      unique(
        identify_pitch_cards$color
      ), function(x) {
        cbind(pitchable_cards %>%
          filter(!!rlang::sym(toupper(x))) %>%
          group_by(sep_side) %>%
          summarise(sum = sum(quantite), .groups = "drop") %>%
          select(-sep_side), name = c("number_pitachabel", "max_add")) %>%
          pivot_wider(values_from = sum, names_from = name) %>%
          mutate(color = x)
      }
    ) %>%
      bind_rows() %>%
      right_join(
        identify_pitch_cards,
        by = "color"
      ) %>%
      distinct(cards, .keep_all = TRUE) %>%
      group_by(
        number_pitachabel, max_add,
        # color,
        number_pitch, quantite
      ) %>%
      summarise(cards = paste0(cards, collapse = "/"), .groups = "drop") %>%
      mutate(cards = paste0(cards, " : ", quantite))


    list_of_result_table <- lapply(
      1:nrow(pitch_cards_and_number_of_pitachable),
      function(x) {
        # cbind(Number_of_pitch_cards = pitch_cards_and_number_of_pitachable[1,]$number_pitachabel,
        generate_res_table(
          deck = sum(test_deck_list %>% filter(!sep_side) %>% pull(quantite)),
          number_pitachabel = pitch_cards_and_number_of_pitachable[x, ]$number_pitachabel,
          max_add = pitch_cards_and_number_of_pitachable[x, ]$max_add,
          quantite = pitch_cards_and_number_of_pitachable[x, ]$quantite,
          number_pitch = pitch_cards_and_number_of_pitachable[x, ]$number_pitch
        ) # )
      }
    ) %>%
      set_names(pitch_cards_and_number_of_pitachable$cards)
  }
  return(list_of_result_table)
}



generate_res_table <- function(
    deck,
    number_pitachabel,
    max_add,
    quantite,
    number_pitch) {
  table_result_base <- expand.grid(
    Draw = seq(7, 7 + 5),
    base_Post_side = seq(
      number_pitachabel - (15 - max_add),
      number_pitachabel + max_add
    ),
    base_pitch_cards = quantite
  ) %>%
    rownames_to_column()


  table_result_expand <-
    right_join(
      table_result_base,
      table_result_base %>%
        group_by(rowname) %>%
        expand(
          Post_side = 0:base_Post_side,
          pitch_cards = 0:base_pitch_cards
        ),
      by = "rowname"
    ) %>%
    select(-rowname) %>%
    mutate(
      other = Draw - (Post_side + pitch_cards)
    ) %>%
    filter(other >= 0)



  table_result_A_and_B <- table_result_expand %>%
    filter(pitch_cards > 0 &
      ((Post_side + pitch_cards - 1) >= number_pitch)) %>%
    select(-other) %>%
    rowwise() %>%
    # proba de toucher elem et pitch
    mutate(
      A_and_B =
        proba_compute(
          success = pitch_cards,
          hit = base_pitch_cards,
          deck = deck,
          sample_size = Draw,
          more_than = FALSE
        ) *
          proba_compute(
            success = Post_side,
            hit = base_Post_side,
            deck = deck - base_pitch_cards,
            sample_size = Draw - pitch_cards,
            more_than = FALSE
          )
    ) %>%
    group_by(Draw, base_Post_side, base_pitch_cards) %>%
    summarise(A_and_B = sum(A_and_B), .groups = "drop")


  table_result_B_knowing_A <- table_result_expand %>%
    mutate(pitch_cards = pitch_cards - 1) %>%
    filter(
      pitch_cards >= 0 &
        ((Post_side + pitch_cards) >= number_pitch)
    ) %>%
    select(-other) %>%
    rowwise() %>%
    mutate(
      B_knowing_A =
        proba_compute(
          success = pitch_cards,
          hit = base_pitch_cards - 1,
          deck = deck - 1,
          sample_size = Draw - 1,
          more_than = FALSE
        ) *
          proba_compute(
            success = Post_side,
            hit = base_Post_side,
            deck = deck - base_pitch_cards,
            sample_size = Draw - (pitch_cards + 1),
            more_than = FALSE
          )
    ) %>%
    group_by(Draw, base_Post_side, base_pitch_cards) %>%
    summarise(B_knowing_A = sum(B_knowing_A), .groups = "drop")



  table_result_final <- inner_join(
    table_result_A_and_B,
    table_result_B_knowing_A,
    by = join_by(Draw, base_Post_side, base_pitch_cards)
  ) %>%
    ungroup() %>%
    mutate(proba = paste(round(A_and_B, 3) * 100, "/", round(B_knowing_A, 3) * 100)) %>%
    select(-A_and_B, -B_knowing_A) %>%
    mutate(Draw = paste0("Turn ", Draw - 6)) %>%
    pivot_wider(
      names_from = Draw,
      values_from = proba
    ) %>%
    mutate(Diff_post_side = base_Post_side - number_pitachabel, .before = 1) %>%
    mutate(Number_of_pitch_cards = number_pitachabel + Diff_post_side, .before = 1) %>%
    select(-base_Post_side)


  return(table_result_final)
}



################################################################################
wrenn_curve <- function(number_of_fetch,
                        number_of_wrenn_fun,
                        deck = 60,
                        card_draw = 8) {
  df_success_wrenn <- rbind(
    expand.grid(
      sucess_wrenn = 0:number_of_wrenn_fun,
      sucess_fetch = 1:number_of_fetch
    ),
    data.frame(
      sucess_wrenn = 0,
      sucess_fetch = 0
    )
  ) %>%
    filter(rowSums(.) <= card_draw)
  # browser()

  result <- df_success_wrenn %>%
    rowwise() %>%
    mutate(
      proba_wrenn =
        proba_compute(
          success = sucess_wrenn,
          hit = number_of_wrenn_fun,
          deck = deck,
          sample_size = card_draw
        ),
      draw = card_draw - sucess_wrenn,
      deck - number_of_wrenn_fun,
      hit = number_of_fetch,
      proba_fetch = proba_compute(
        success = sucess_fetch,
        hit = number_of_fetch,
        deck = deck - number_of_wrenn_fun,
        sample_size = card_draw - sucess_wrenn
      ),
      proba = proba_wrenn *
        proba_fetch
    )

  return(sum(result$proba))
}


Wrenn_fun_analysis <- function(deck_list_path,
                               card_DB) {
  test_deck_list <- deck_parser(deck_list_path)





  Fetch_land <- card_DB %>%
    filter(str_detect(tolower(type_line), "land")) %>%
    select(name, oracle_text, image_uris.large) %>%
    filter(str_detect(tolower(oracle_text), "^\\{t\\}, pay 1 life, sacrifice")) %>%
    mutate(fetchable_land = str_extract(
      tolower(oracle_text),
      "(?<=\\: search your library for .{1,2}\\s)(.+)(?=card, put it onto the battlefield, then shuffle\\.)"
    )) %>%
    separate_wider_delim(fetchable_land,
      " or ",
      names = c("A", "B"),
      too_few = c("align_start")
    ) %>%
    select(-oracle_text) %>%
    pivot_longer(-c(name, image_uris.large), names_to = "temp", values_to = "type_fetchable") %>%
    mutate(type_fetchable = trimws(type_fetchable, which = "both")) %>%
    select(-temp) %>%
    drop_na() %>%
    dplyr::rename(
      fetch_name = name,
      fetch_image = image_uris.large
    ) %>%
    mutate(fetch_name = tolower(fetch_name))





  Number_of_fetch_in_decklist <- test_deck_list %>%
    inner_join(
      Fetch_land %>%
        distinct(fetch_name),
      by = c("nom" = "fetch_name")
    )



  number_of_wrenn <- test_deck_list %>%
    filter(nom == "wrenn and six") %>%
    pull(quantite)





  if (length(number_of_wrenn) < 1 | nrow(Number_of_fetch_in_decklist) < 1) {
    fun_result <- NULL
  } else {
    data_plot_wrenn <- expand.grid(
      n_fetch = 7:20,
      draw = 8 + (0:5)
    ) %>%
      rowwise() %>%
      mutate(
        proba = wrenn_curve(
          number_of_fetch = n_fetch,
          card_draw = draw,
          number_of_wrenn_fun = number_of_wrenn
        ) #* 100
      ) %>%
      ungroup() %>%
      mutate(draw = paste(
        "Draw naturel +",
        draw - 8
      ))



    res_table <- data_plot_wrenn %>%
      filter(draw == "Draw naturel + 0", n_fetch > 6, n_fetch < 17) %>%
      select(-draw) %>%
      mutate(proba = paste0(round(proba, 2), " %"))



    p <- ggplot(data_plot_wrenn) +
      geom_line(aes(x = n_fetch, y = proba, colour = draw)) +
      geom_vline(
        xintercept = sum(Number_of_fetch_in_decklist$quantite),
        linetype = "dotted",
        color = "blue", linewidth = 1.5
      )

    fun_result <- list(
      table = res_table,
      plot = p
    )
  }
  
  return(
    fun_result
  )
}

################################################################################
