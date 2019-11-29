DOCKER_USER=letrov
DOCKER_IMAGE=sting
DOCKER_TAG=latest

docker:
	docker build -t $(DOCKER_IMAGE) -f ci/Dockerfile . && \
	docker tag $(DOCKER_IMAGE) $(DOCKER_USER)/$(DOCKER_IMAGE):$(DOCKER_TAG) && \
	docker push $(DOCKER_USER)/$(DOCKER_IMAGE):$(DOCKER_TAG)
