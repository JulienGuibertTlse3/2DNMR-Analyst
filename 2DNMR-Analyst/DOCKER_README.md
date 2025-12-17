# Docker Deployment for 2DNMR-Analyst

## Quick Start

### Option 1: Docker Compose (Recommended)
```bash
# Build and run
docker-compose up -d

# View logs
docker-compose logs -f

# Stop
docker-compose down
```

### Option 2: Docker CLI
```bash
# Build the image
docker build -t 2dnmr-analyst .

# Run the container
docker run -d -p 3838:3838 --name 2dnmr-analyst 2dnmr-analyst

# Stop
docker stop 2dnmr-analyst
```

## Access the Application
Open your browser and go to: **http://localhost:3838**

## Requirements
- Docker Desktop (Windows/Mac) or Docker Engine (Linux)
- ~4 GB disk space for the image
- ~4 GB RAM recommended

## Troubleshooting

### View container logs
```bash
docker logs 2dnmr-analyst
```

### Enter the container for debugging
```bash
docker exec -it 2dnmr-analyst /bin/bash
```

### Rebuild after changes
```bash
docker-compose up -d --build
```

### Check R packages inside container
```bash
docker exec -it 2dnmr-analyst R -e "installed.packages()[,1]"
```

### Check Python/TensorFlow
```bash
docker exec -it 2dnmr-analyst python3 -c "import tensorflow; print(tensorflow.__version__)"
```

## Customization

### Change the port
Edit `docker-compose.yml`:
```yaml
ports:
  - "8080:3838"  # Access via localhost:8080
```

### Mount local data
Edit `docker-compose.yml`:
```yaml
volumes:
  - /path/to/your/spectra:/srv/shiny-server/2dnmr-analyst/data
```

## Files Required
Make sure these files are in your project root:
- `Dockerfile`
- `docker-compose.yml`
- `.dockerignore`
- `renv.lock`
- `.Rprofile`
- `renv/` folder (with activate.R)
- Your app files (app.R or ui.R/server.R)
