FROM mcr.microsoft.com/azure-cli@sha256:3bd47b60d43ac2b82c13e9045cec07a7240f9e52295ab825d9e0f86458738aad
RUN curl -LO https://storage.googleapis.com/kubernetes-release/release/v1.19.0/bin/linux/amd64/kubectl && \
    chmod +x ./kubectl && \
    mv ./kubectl /bin/kubectl
