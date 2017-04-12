#---------------------------->
#--- Summarise Results
#--- Alwin Wang MAE3401
#============================>

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

savedata = "Output_Data"
savesummary = "Summary_Data"

filelist = list.files(savedata, pattern = "*.rds")

filecontents <- 
  data.frame(filename = filelist, 
             temp = gsub("Re", "_", gsub("AoA", "_", gsub(".rds", "_", filelist)))) %>% 
  separate(temp, c("temp1", "Re", "AoA", "Data", "temp2"), sep = "_") %>%
  select(-temp1, - temp2) %>%
  mutate(Re = as.numeric(Re)) %>%
  mutate(AoA = as.numeric(AoA))


#################################
dataname = "blplot"

datasum = lapply(
  filter(filecontents, Data == dataname)$filename,
  function(filename) {
    data = data.frame(readRDS(paste0(savedata, "/", filename)))
    ID = filecontents[filecontents$filename == filename,] %>% 
      select(Re, AoA)
    return(data.frame(data, ID))
  }
)
datasum <- bind_rows(datasum)

ticks = c(1, 3, 5, 8, 10)
ooms = 10^(-3:1)
breaks = as.vector(ticks %o% ooms)

show.labels = c(T, T, F, F, T)
labels = as.character(breaks * show.labels)
labels = gsub("^0$", "", labels)

dataplot <- datasum %>%
  mutate(Re = paste("Re", Re),
         surf = factor(surf, levels = c("upper", "lower"), labels = c("upper", "lower")))

### PLOT OF COMBINED LOG-LOG BL
ggplot(data = filter(dataplot, method %in% c("99% Max"), Re %in% c("Re 200", "Re 800")), 
       aes(x = (x + 0.5 - 0.001), y = dist, group = interaction(surf, method, Re, AoA),
           colour = AoA)) + 
  geom_path() +
  geom_path(
    data = filter(dataplot, method %in% c("Blasius"), Re %in% c("Re 200", "Re 800")),
    aes(linetype = "Blasisus"), colour = "black", size = 0.5
  ) +
  scale_y_log10(limits = c(0.005, 3), breaks = breaks, labels = labels) +
  scale_x_log10(limits = c(1e-3, 1), breaks = breaks, labels = labels) +
  scale_linetype_manual("Theory", values = "5222") +
  labs(title = "Boundary Layer",
       y = "Thickness", x = "Length Along Chord") + 
  facet_grid(surf ~ Re) + 
  scale_colour_gradient(low = "#E5E5E5", high = "#313131")
  

dataplot <- datasum %>%
  mutate(AoA = paste("AoA", AoA),
         surf = factor(surf, levels = c("upper", "lower"), labels = c("upper", "lower")))

ggplot(data = filter(dataplot, method %in% c("99% Max"), AoA %in% c("AoA -10", "AoA 0", "AoA 10")), 
       aes(x = (x + 0.5 - 0.001), y = dist, group = interaction(surf, method, Re, AoA),
           colour = log10(Re))) + 
  geom_path() +
  geom_path(
    data = filter(dataplot, method %in% c("Blasius"), AoA %in% c("AoA -10", "AoA 0", "AoA 10")),
    aes(linetype = "Blasisus", colour = log10(Re)), size = 0.5
  ) +
  scale_y_log10(limits = c(0.001, 15), breaks = breaks, labels = labels) +
  scale_x_log10(limits = c(0.005, 1), breaks = breaks, labels = labels) +
  scale_linetype_manual("Theory", values = "5222") +
  labs(title = "Boundary Layer",
       y = "Thickness", x = "Length Along Chord") + 
  facet_grid(surf ~ AoA) + 
  scale_colour_gradient(expression(log[10](Re)), low = "#D5D5D5", high = "#313131")



### PLOT OF VP -----
dataname = "velprofile"
velprofile = lapply(
  filter(filecontents, Data == dataname)$filename,
  function(filename) {
    data = data.frame(readRDS(paste0(savedata, "/", filename)))
    ID = filecontents[filecontents$filename == filename,] %>% 
      select(Re, AoA)
    return(data.frame(data, ID))
  }
)
velprofile <- bind_rows(velprofile)

dataname = "vptheory"
vptheory = lapply(
  filter(filecontents, Data == dataname)$filename,
  function(filename) {
    data = data.frame(readRDS(paste0(savedata, "/", filename)))
    ID = filecontents[filecontents$filename == filename,] %>% 
      select(Re, AoA)
    return(data.frame(data, ID))
  }
)
velprofile <- bind_rows(vptheory)

velprofileplot = filter(velprofile, Re == 50, AoA == 0)
vptheoryplot = filter(vptheory, Re == 50, AoA == 0)


ashift = -0.5
ggplot(data = velprofileplot, 
                  aes(x = ifelse(surf == "upper", 1, -1) * dist,
                      group = interaction(surf, xO))) +
  # Airfoil
  geom_path(data = data.frame(x = c(0.001, 0.001, -0.001, -0.001), y = c(-0.5, 0.5, 0.5, -0.5)), 
            aes(x = x, y = y - ashift, linetype = "Aerofoil Surface"), 
            colour = "black", size = 0.5,
            inherit.aes = FALSE) +
  # Flow in BL
  geom_ribbon(aes(ymin = xO - ashift, ymax = xO + UUmdash * sep / 1.5 - ashift, alpha = "out")) +
  # Flow out of BL
  geom_ribbon(data = filter(velprofileplot, bl == TRUE), 
              aes(ymin = xO - ashift, ymax = xO + UUmdash * sep / 1.5 - ashift, alpha = "in")) +
  # Blasius soln for velocity profiles
  geom_path(data = vptheoryplot, aes(y = xO + UUmblasius * sep/1.5 - ashift),
            linetype = "5111") +
  # Boundary Layers
  geom_path(data = filter(blplot, method %in% c("Blasius", "99% Max")), 
            aes(y = xO - ashift,
                group = interaction(method),
                colour = method)) +
  # Legends
  scale_alpha_manual("Flow", values = c(0.3, 0.1), labels = c("Boundary Layer", "Free Stream")) +
  scale_linetype_manual("Airfoil", values = c("solid"), labels = c("Surface")) + 
  # scale_linetype_manual("Boundary Layer", values = c("dashed", "solid")) +
  # Plot transformations & Labels
  coord_flip(xlim = c(-distmax, distmax), ylim = c(-0.1, 1.2)) +
  theme(aspect.ratio = 0.6) 

+
  labs(title = paste("Re Number", Re, "AoA", paste(AoA, "deg:", sep = ""), "Velocity Profiles"),
       x = "Distance Along the Chord", y = "Distance from the Leading Edge")



### PLOT OF VP ----
dataname = "airfoilsurfmesh"
airfoilsurfmesh = lapply(
  filter(filecontents, Data == dataname)$filename,
  function(filename) {
    data = data.frame(readRDS(paste0(savedata, "/", filename)))
    ID = filecontents[filecontents$filename == filename,] %>% 
      select(Re, AoA)
    return(data.frame(data, ID))
  }
)
airfoilsurfmesh <- bind_rows(airfoilsurfmesh) %>%
  mutate(ID = paste("Re", Re, "AoA", AoA)) %>%
  mutate(Cp = 2 * P,
         chord = xO2chord(xO, surf))


Replot = 50
airfoilsurfmeshplot = filter(airfoilsurfmesh, Re == Replot)

scene = list(camera = list(eye = list(x = 1.25, y = -1.25, z = 0.75)))
plot_Cp <- plot_ly(airfoilsurfmeshplot, x = ~chord, y = ~AoA, z = ~Cp, type = 'scatter3d', mode = 'lines',
             opacity = 1, color = ~AoA) %>% 
  layout(title = paste("Coefficient of Pressure at Re", Replot), scene = scene)

scene = list(camera = list(eye = list(x = 1.5, y = -1.5, z = 0.75)))
plot_Vort <- plot_ly(airfoilsurfmeshplot, x = ~chord, y = ~AoA, z = ~vort_xy_plane, type = 'scatter3d', mode = 'lines',
                   opacity = 1, color = ~AoA) %>% 
  layout(title = paste("Vorticity at Re", Replot), scene = scene)
