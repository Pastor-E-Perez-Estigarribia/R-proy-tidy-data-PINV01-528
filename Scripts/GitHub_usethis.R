pacman::p_load("usethis")

# 1. Crea un Token de acceso (te llevará a la web de GitHub)
create_github_token() 

# 2. Guarda ese token cuando RStudio te lo pida
gitcreds::gitcreds_set()

# 3. Convierte tu carpeta en un repositorio de GitHub
use_github()