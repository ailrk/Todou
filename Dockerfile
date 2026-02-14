# Stage 1: Build
FROM haskell:9.6 AS builder

WORKDIR /opt/todou

# Copy the cabal file first
COPY *.cabal ./

# Build ONLY the external library dependencies
# This is the "Slow" layer that gets cached.
RUN cabal update && cabal build --only-dependencies -j

# NOW copy your internal assets and source
# We copy 'data' and 'src' so Template Haskell can find them.
COPY . .

# DEBUG: Check if data/todou/main.js exists here
RUN ls -l data/todou/main.js

# Build the actual application
# Since dependencies are already cached, this will be fast.
RUN cabal install --install-method=copy --installdir=./bin

# --- Stage 2: Runtime ---
FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y \
    ca-certificates libsqlite3-0 libgmp10 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /opt/todou/bin/todou /usr/local/bin/todou

# Ensure the app can run
EXPOSE 5000
ENTRYPOINT ["todou"]
CMD ["--port=5000", "--storage=dir:/data"]
