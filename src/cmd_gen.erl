-module(cmd_gen).

-export([docker_python/2, docker_kill/1]).

docker_python(AbsoluteDir, ContainerName) ->
    "timeout --signal=SIGKILL 30 docker run --interactive --rm --name "
    ++ ContainerName
    ++ " --ulimit cpu=1 --memory=20m -v '"
    ++ AbsoluteDir
    ++ ":/app' -w /app python python -u "
    ++ "main.py".

docker_kill(ContainerName) ->
    "docker kill " ++ ContainerName.
