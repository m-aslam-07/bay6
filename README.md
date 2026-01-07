# Runner – Docker Setup Guide

This repository contains the **Runner service** for the
**Evolutionary Algorithms On Click** platform.

The runner depends on several backend services (database, message queue,
object storage, auth, etc.) that are defined in the
`Evolutionary-Algorithms-On-Click/operations` repository.

This guide explains, step by step, how to run the full system using Docker,
while replacing the default runner with the updated version from this repository.

---

## Prerequisites

Make sure the following are installed on your system:

- Docker
- Docker Compose
- Git

Verify installation:

```bash
docker --version
docker compose version
git --version
