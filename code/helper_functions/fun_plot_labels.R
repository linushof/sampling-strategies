custom_theme <-   theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_line(linewidth = .25) , 
        panel.grid.major = element_line(linewidth = .25))

# plot labels

label_theta <- function(string) {
  TeX(paste("$\\theta=$", string, sep = ""))
}

label_gamma <- function(string) {
  TeX(paste("Curvature", "$\\gamma$"))
}

label_delta <- function(string) {
  TeX(paste("Elevation", "$\\delta$"))
}

label_alpha <- function(string) {
  TeX(paste("Sensitivity", "$\\alpha$"))
}

label_pi <- function(string) {
  paste("Probability Function")
}

label_v <- function(string) {
  paste("Value Function")
}


label_psi <- function(string) {
  TeX(paste("$\\psi=$", string, sep = ""))
}

# probability of high risky outcome
label_rare <- function(string) {
  TeX(paste("$\\p_{high}\\in$", string, sep = "")) 
}