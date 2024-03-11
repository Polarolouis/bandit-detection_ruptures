necessary_packages <- c("dplyr", "here", "remotes", "ggplot2", "latex2exp", "patchwork", "CptPointProcess")

if (any(!(necessary_packages %in% installed.packages()))) {
    # Si un des packages est manquant on les installe
    install.packages(necessary_packages)

    # Sauf que le package de détection des pts de ruptures n'est pas sur le CRAN
    # Donc on le récupère et on l'installe
    remotes::install_url("https://raw.githubusercontent.com/ELebarbier/CptPointProcess/main/CptPointProcess_0.0.0.1.tar.gz")
}

require("CptPointProcess", quietly = TRUE)
require("dplyr")
require("here")
require("ggplot2")
require("patchwork")
require("latex2exp")

for (file in list.files(path = here("data"), pattern = "*.csv$")) {
    # Chargement des données
    soub_data <- read.csv(here("data", file))

    soub_data[["id_clic"]] <- seq_len(nrow(soub_data))

    # TODO Discuter si pour les temps, il ne faudrait pas soustraire le premier ramener à 0
    # le temps de commencement, sinon commenter la ligne ci-dessous
    soub_data[["time"]] <- soub_data[["time"]] - min(soub_data[["time"]] + 1e-6)
    # Le 1e-6 sert à éviter le 0, car la méthode ne veut ni 0 ni 1

    max_soub_time <- max(soub_data[["time"]])

    soub_data[["time"]] <- soub_data[["time"]] / max_soub_time

    # Application de la méthode
    result_soub <- CptPointProcess(
        ProcessData =
            data.frame(times = soub_data[["time"]][-length(soub_data[["time"]])]), 
            selection = TRUE
    )

    # Récupération des points de ruptures
    rupture_point_begin <- result_soub[["SegK"]][["begin"]]
    lambdas <- result_soub[["SegK"]][["lambda"]]

    # Récupération des instants REE depuis le jeu de données
    soub_data <- soub_data %>% mutate(
        isBlackSwan = (gain == -3000L),
        isJackpot = (gain == 3000L),
        lambda = sapply(time, function(t) {
            last(lambdas[rupture_point_begin <= t])
        }),
    )

    # Affichage

    plot_clicks <- ggplot(soub_data) +
        geom_point(aes(x = time, y = id_clic), alpha = 0.25) +
        geom_line(aes(x = time, y = id_clic)) +
        # On ajoute les jackpots
        geom_vline(xintercept = .data[["time"]][.data[["isJackpot"]]], color = "green") +
        # On ajoute les blackswan
        geom_vline(xintercept = .data[["time"]][.data[["isBlackSwan"]]], color = "red") +
        # On ajoute les points de ruptures
        geom_vline(xintercept = rupture_point_begin, color = "blue") +
        xlab("Temps normalisé") +
        ylab("Nombre de clics") +
        theme_bw() +
        theme(aspect.ratio = 1)


    plot_lambdas <- ggplot(soub_data) +
        geom_step(aes(x = time, y = lambda), color = "violet") +
        # On ajoute les jackpots
        geom_vline(xintercept = .data[["time"]][.data[["isJackpot"]]], color = "green") +
        # On ajoute les blackswan
        geom_vline(xintercept = .data[["time"]][.data[["isBlackSwan"]]], color = "red") +
        # On ajoute les points de ruptures
        geom_vline(xintercept = rupture_point_begin, color = "blue") +
        xlab("Temps normalisé") +
        ylab(TeX("$\\lambda$, intensité")) +
        theme_bw() +
        theme(aspect.ratio = 1)

    # TODO Choisir l'affichage
    # Pour Lola : Choix 1
    plot_cote_a_cote <- plot_clicks + plot_lambdas

    # Pour Lola : Choix 2
    # Triche pour afficher les deux plots sur le même graph
    coeff <- max(soub_data[["id_clic"]]) / max(lambdas)

    plot_combine <- ggplot(soub_data) +
        geom_point(aes(x = time, y = id_clic), alpha = 0.25) +
        geom_line(aes(x = time, y = id_clic)) +
        geom_step(aes(x = time, y = lambda * coeff), color = "violet") +

        # On ajoute les jackpots
        geom_vline(xintercept = .data[["time"]][.data[["isJackpot"]]], color = "green") +
        # On ajoute les blackswan
        geom_vline(xintercept = .data[["time"]][.data[["isBlackSwan"]]], color = "red") +
        # On ajoute les points de ruptures
        geom_vline(xintercept = rupture_point_begin, color = "blue") +
        scale_y_continuous(
            # Features of the first axis
            name = "Nombre de clics",
            # Add a second axis and specify its features
            sec.axis = sec_axis(~ . / coeff,
                name = TeX("$\\lambda$, intensité")
            )
        ) +
        xlab("Temps normalisé") +
        theme_bw() +
        theme(aspect.ratio = 1)

    plot_cote_a_cote
    plot_combine
    # TODO Une fois choisi le plot préféré
    ggsave(
        plot = plot_combine,
        filename = here("img", paste0("ruptures_", file, ".jpg")),
        bg = "white"
    )
}
