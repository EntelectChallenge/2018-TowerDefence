FROM openjdk:latest
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && apt-get install -y sudo make && \
    adduser --disabled-password --gecos '' entelect && \
    usermod -aG sudo entelect && \
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
COPY starter-pack /home/entelect/starter-pack
RUN chown -R entelect:entelect /home/entelect/starter-pack
USER entelect
WORKDIR /home/entelect/starter-pack
