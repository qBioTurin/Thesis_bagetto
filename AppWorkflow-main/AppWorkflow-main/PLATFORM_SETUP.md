# Cross-Platform Docker Compose Setup

This project supports **Linux**, **macOS**, and **Windows** using platform-specific Docker Compose overrides.

## Quick Start by OS

### Linux
```bash
docker-compose -f docker-compose.yml -f docker-compose.override.linux.yml up -d
```

Or make it default (one-time):
```bash
cp docker-compose.override.linux.yml docker-compose.override.yml
docker-compose up -d
```

**Available paths for output folders:** `/run/media/`, `/mnt/`, `/media/`, `${HOME}`

---

### macOS
```bash
docker-compose -f docker-compose.yml -f docker-compose.override.macos.yml up -d
```

Or make it default (one-time):
```bash
cp docker-compose.override.macos.yml docker-compose.override.yml
docker-compose up -d
```

**Available paths for output folders:** 
- `${HOME}` (usually `/Users/<username>`)
- `/Volumes/` (external drives and mounted volumes)

**Note:** Docker Desktop on macOS runs Linux in a lightweight VM, so `/var/run/docker.sock` works inside containers.

---

### Windows (Docker Desktop)
```bash
docker-compose -f docker-compose.yml -f docker-compose.override.windows.yml up -d
```

Or make it default (one-time):
```bash
copy docker-compose.override.windows.yml docker-compose.override.yml
docker-compose up -d
```

**Available paths for output folders:**
- Windows drive letters are auto-mapped: `C:\Users\<username>` → `/c/users/<username>` inside container
- For file access, use the **browser file picker** in the Shiny app (recommended)
- Or select via `${HOME}` which maps to your user directory

**Note:** On Windows, you may need to configure file sharing in Docker Desktop settings:
- Settings → Resources → File Sharing
- Add `C:\Users\<your_username>` if not already listed

---

## Environment Variables

The compose files use:
- `${HOME}` - User home directory (cross-platform)
- `${COMPOSE_PROJECT_NAME}` - Project name (optional, defaults to folder name)

---

## Troubleshooting

### "Permission denied" when writing to output folder
- **Linux:** Ensure the folder has write permissions
- **macOS:** Check Docker Desktop file sharing settings (Preferences → Resources → File Sharing)
- **Windows:** Check Docker Desktop file sharing (Settings → Resources → File Sharing)

### "Cannot connect to Docker daemon"
- **Linux:** Ensure Docker daemon is running: `sudo systemctl start docker`
- **macOS/Windows:** Ensure Docker Desktop is running

### "Port 3838 already in use"
Change the port in `docker-compose.yml`:
```yaml
ports:
  - "3839:3838"  # Use 3839 instead
```

---

## Platform-Specific Notes

| Feature | Linux | macOS | Windows |
|---------|-------|-------|---------|
| Docker Socket | `/var/run/docker.sock` | VM-managed | Docker Desktop |
| Home Directory | `${HOME}` | `${HOME}` | `${HOME}` |
| External Drives | `/mnt/`, `/media/` | `/Volumes/` | Auto-mapped |
| File Performance | ✅ Native | ⚠️ Slower (VM) | ⚠️ Slower (VM) |
| Recommended FS | ext4 | APFS | NTFS |

---

## Creating Your Own Override

If you need custom mounts, create a `docker-compose.override.yml` in the project root:

```yaml
services:
  app:
    volumes:
      - /path/to/custom:/container/path
      - /var/run/docker.sock:/var/run/docker.sock
```

Docker Compose automatically uses `docker-compose.override.yml` if it exists.
