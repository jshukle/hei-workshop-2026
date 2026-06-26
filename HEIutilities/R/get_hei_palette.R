#' Get a Health Effects Institute color palette
#'
#' Returns one of the color vectors used in HEI and State of Global Air workshop
#' graphics. Palettes may be unnamed sequential color vectors, named categorical
#' color vectors, or a single color value used for bars, labels, or map text.
#'
#' @param palette Character string naming the palette to return. Use
#'   `"soga_2025_available"` to list the primary State of Global Air 2025
#'   palette names. Other supported values include `"soga_2025_6"`,
#'   `"soga_2025_5"`, `"soga_2025_4"`, `"soga_2025_gbd_spr_rgns"`,
#'   `"soga_2025_gbd_spr_rgns_abbr"`,
#'   `"soga_2025_gbd_spr_rgns_abbr_brks"`, `"soga_2025_bar_positive"`,
#'   `"soga_2025_bar_negative"`, `"soga_2025_bar_blue1"`,
#'   `"soga_2025_bar_blue2"`, `"soga_2025_bar_blue3"`, `"soga_2025_text"`,
#'   `"hei_cut_grid_6"`, `"hei_pal6"`, `"hei_pal5"`, `"hei_pal4"`,
#'   `"hei_pal5_v2"`, `"hei_5_purple"`, `"hei_5_blue"`, `"hei_6_blue"`,
#'   `"pm25_5"`, `"ozone_5"`, `"hap_5"`, `"no2_5"`, `"neonatal_5"`,
#'   `"other_5_1"`, `"other_5_2"`, `"gbd_super_regions"`,
#'   `"sa_regions"`, `"south_asia_palette"`, `"central_asia_palette"`,
#'   `"southeast_asia_palette"`, `"bar_blue"`, `"bar_orange"`,
#'   `"bar_yellow"`, `"bar_gray"`, or `"fuel_type"`.
#'
#' @return A character vector of hexadecimal color values. Some categorical
#'   palettes are named vectors. Single-color palettes return a length-one
#'   character vector.
#'
#' @examples
#' get_hei_palette("soga_2025_available")
#' get_hei_palette("soga_2025_6")
#' get_hei_palette("fuel_type")
#'
#' @export
get_hei_palette <- function(palette) {

  # SoGA 2025 color palette

  if(palette == "soga_2025_available") {
    out <- c("soga_2025_6", "soga_2025_5", "soga_2025_gbd_spr_rgns", "soga_2025_gbd_spr_rgns_abbr", "soga_2025_bar_positive",
    "soga_2025_bar_negative", "soga_2025_bar_blue1", "soga_2025_bar_blue2", "soga_2025_bar_blue3", "soga_2025_text")
  }

  if(palette == "soga_2025_6") {
    out <- c("#216999", "#3b94c2", "#d1e5f0", "#dcb151", "#f5a382", "#d96354", "#B2B2B2")
  }

  if(palette == "soga_2025_5") {
    out <- c("#216999", "#9eccf2", "#dcb151", "#f5a382", "#d96354", "#B2B2B2")
  }

  if(palette == "soga_2025_4") {
    out <- c("#216999", "#9eccf2", "#dcb151", "#d96354", "#B2B2B2")
  }

  if(palette == "soga_2025_gbd_spr_rgns") {
    out <- c(
      "Global" = "#000000",
      "South Asia" = "#aa7d24",
      "East, West, Central, and Southern Africa" = "#216999",
      "Southeast Asia, East Asia, and Oceania" = "#dcb151",
      "Latin America and Caribbean" = "#9eccf2",
      "North Africa and Middle East" = "#d86354",
      "Central Europe, Eastern Europe, and Central Asia" = "#1f42a3",
      "High-income" = "#6696c9",
      "No Data" = "#B2B2B2"
    )
  }

  if(palette == "soga_2025_gbd_spr_rgns_abbr") {
    out <- c(
      "Global" = "#000000",
      "South Asia" = "#aa7d24",
      "East, West, Cent., & Southern Africa" = "#216999",
      "Southeast Asia, East Asia, & Oceania" = "#dcb151",
      "Latin America & Caribbean" = "#9eccf2",
      "North Africa & Middle East" = "#d86354",
      "Cent. Europe, E. Europe, & Cent. Asia" = "#1f42a3",
      "High-income" = "#6696c9",
      "No Data" = "#B2B2B2"
    )
  }

  if(palette == "soga_2025_gbd_spr_rgns_abbr_brks") {
    out <- c(
      "Global" = "#000000",
      "South Asia" = "#aa7d24",
      "East, West, Cent.,\n & Southern Africa" = "#216999",
      "Southeast Asia,\n East Asia, & Oceania" = "#dcb151",
      "Latin America\n & Caribbean" = "#9eccf2",
      "North Africa\n & Middle East" = "#d86354",
      "Cent. Europe, E. Europe,\n & Cent. Asia" = "#1f42a3",
      "High-income" = "#6696c9",
      "No Data" = "#B2B2B2"
    )
  }

  if(palette == "soga_2025_bar_positive") {
    out <- "#9eccf2"
  }

  if(palette == "soga_2025_bar_negative") {
    out <- "#f5a382"
  }

  if(palette == "soga_2025_bar_blue1") {
    out <- "#90c7d6"
  }

  if(palette == "soga_2025_bar_blue2") {
    out <- "#356171"
  }

  if(palette == "soga_2025_bar_blue3") {
    out <- "#113942"
  }

  if(palette == "soga_2025_text") {
    out <- "#a7a9ac"
  }

  # Prior HEI color palettes

  if(palette == "hei_cut_grid_6") {
    out <- c("#147218","#64AA0F", "#C3E218", "#FBDE16", "#FE990D", "#FE3C19", "#B2B2B2")
  }

  if(palette == "hei_pal6") {
    out <- c("#128aaf","#66d3be", "#f6f300", "#f68f00", "#f61700", "#772424", "#B2B2B2")
  }

  if(palette == "hei_pal5") {
    out <- c("#128aaf","#66d3be", "#f6f300", "#f68f00", "#f61700", "#B2B2B2")
  }

  if(palette == "hei_pal4") {
    out <- c("#128aaf","#66d3be", "#f68f00", "#f61700", "#B2B2B2")
  }

  if(palette == "hei_pal5_v2") {
    out <- c("#66d3be", "#128aaf", "#f6f300", "#f68f00", "#f61700", "#B2B2B2")
  }

  if(palette == "hei_5_purple") {
    out <- c("#FEEBE2", "#FCBCBD", "#F985AB", "#D23490", "#7A0177", "#B2B2B2")
  }

  if(palette == "hei_5_blue") {
    out <- c("#F0F9E8", "#98D5C0", "#4BA8C9", "#1D79B5", "#254B8C", "#B2B2B2")
  }

  if(palette == "hei_6_blue") {
    out <- c("#F0F9E8", "#98D5C0", "#4BA8C9", "#1D79B5", "#254B8C","#172e57", "#B2B2B2")
  }

  if(palette == "pm25_5") {
    out <- c("#fde1cf", "#fd9079", "#e9634a", "#d03d23", "#9c1b19", "#B2B2B2")
  }

  if(palette == "ozone_5") {
    out <- c("#b0dad2", "#5ec6c3", "#35b1b7", "#00929c", "#017571", "#B2B2B2")
  }

  if(palette == "hap_5") {
    out <- c("#e0deea", "#c2b5ce", "#a78bab", "#856299", "#633d79", "#B2B2B2")
  }

  if(palette == "no2_5") {
    out <- c("#e0deea", "#c2b5ce", "#a78bab", "#856299", "#633d79", "#B2B2B2")
  }

  if(palette == "neonatal_5") {
    out <- c("#E9E5C0", "#C8C598", "#ABA573", "#8B8752", "#6D6B2E", "#B2B2B2")
  }

  if(palette == "other_5_1") {
    out <- c("#e5cccc", "#d4aaaa", "#c38888", "#b26666", "#994d4d", "#B2B2B2")
  }

  if(palette == "other_5_2") {
    out <- c("#ebd5c6", "#deb8a0", "#d19c7a", "#c48055", "#aa673b", "#B2B2B2")
  }

  if(palette == "gbd_super_regions") {
    out <- c(
      "Global" = "#000000",
      "South Asia" = "#D2CC28",
      # "Sub-Saharan Africa" = "#A2562F",
      "East, West, Central, and Southern Africa" = "#A2562F",
      "East, West, Cent.,\n & Southern Africa" = "#A2562F",
      "Southeast Asia, East Asia, and Oceania" = "#E32027",
      "Southeast Asia,\n East Asia, & Oceania" = "#E32027",
      "Latin America and Caribbean" = "#9750A0",
      "Latin America\n & Caribbean" = "#9750A0",
      "North Africa and Middle East" = "#F67F21",
      "North Africa\n & Middle East" = "#F67F21",
      "Central Europe, Eastern Europe, and Central Asia" = "#397EB9",
      "Cent. Europe, E. Europe,\n  & Cent. Asia" = "#397EB9",
      "High-income" = "#4DAF48"
    )
  }

  if(palette == "sa_regions") {
    out <- c(
      "South Asia" = "#A2562F",
      "Central Asia" = "#D2CC28",
      "Southeast Asia" = "#4DAF48"
    )
  }

  if(palette == "south_asia_palette") {
    out <- c(
      "Global" = "#000000",
      "India" = "#a6cee3",
      "Bangladesh" = "#1f78b4",
      "Bhutan" = "#b2df8a",
      "Pakistan" = "#33a02c",
      "Sri Lanka" = "#fb9a99",
      "Afghanistan" = "#e31a1c",
      "The Maldives" = "#fdbf6f",
      "Nepal" = "#6a3d9a",
      "Aksai Chin (India-China, post-1963)" = "#ff7f00",
      "Aksai Chin" = "#ff7f00"
    )
  }

  if(palette == "central_asia_palette") {
    out <- c(
      "Global" = "#000000",
      "Kazakhstan" = "#a6cee3",
      "Kyrgyzstan" = "#1f78b4",
      "Tajikistan" = "#b2df8a",
      "Turkmenistan" = "#33a02c",
      "Uzbekistan" = "#fb9a99"
    )
  }

  if(palette == "southeast_asia_palette") {
    out <- c(
      "Global" = "#000000",
      "Cambodia" = "#a6cee3",
      "Indonesia" = "#1f78b4",
      "Lao People's Democratic Republic" = "#b2df8a",
      "Lao PDR" = "#b2df8a",
      "Malaysia" = "#33a02c",
      "Myanmar" = "#fb9a99",
      "The Philippines" = "#e31a1c",
      "Thailand" = "#fdbf6f",
      "Timor-Leste" = "#ff7f00",
      "Viet Nam" = "#cab2d6",
      "Singapore" = "#6a3d9a"
    )
  }

  if(palette == "bar_blue") {
    out <- "#226273"
  }

  if(palette == "bar_orange") {
    out <- "#F15F27"
  }

  if(palette == "bar_yellow") {
    out <- "#F0C536"
  }

  if(palette == "bar_gray") {
    out <- "#D9D9D8"
  }

  if(palette == "fuel_type") {
    out <- c("Coal" = "#226273", "Oil & Gas" = "#F15F27", "Biofuel" =  "#F0C536", "Other" = "#D9D9D8")
  }

  return(out)
}
