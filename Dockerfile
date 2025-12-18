# Stage 1: Build the application
FROM sbtscala/scala-sbt:eclipse-temurin-jammy-17.0.10_7_1.9.9_3.4.1 AS builder

WORKDIR /app

# Copy the project files
COPY . .

# Build the über-jar using the custom task defined in build.sbt
# This task compiles the code, builds the frontend (Scala.js), copies assets, and packages the server
RUN sbt packageForDeployment

# Stage 2: Run the application
FROM eclipse-temurin:17-jre-jammy

WORKDIR /app

# Copy the built jar from the builder stage
COPY --from=builder /app/apps/jvm/target/scala-3.7.2/webapp.jar ./webapp.jar

# Expose the port (Fly.io defaults to 8080)
EXPOSE 8080

# Run the application
CMD ["java", "-jar", "webapp.jar"]
