[![Open in Visual Studio Code](https://classroom.github.com/assets/open-in-vscode-c66648af7eb3fe8bc4f294546bfd86ef473780cde1dea487d3c4ff354943c9ae.svg)](https://classroom.github.com/online_ide?assignment_repo_id=9711072&assignment_repo_type=AssignmentRepo)
# INFOB3CC Assignment 3: QuickHull

Name: **ENTER YOUR NAME HERE**<br>
Student Number: **ENTER YOUR STUDENT NUMBER HERE**

http://www.cs.uu.nl/docs/vakken/b3cc/assignments.html


## Setup: ghcup

This method will work for those using *nix or MacOS. Windows users are
recommended to follow the instructions for using the Dev Container inside VS
Code.

The repository can be built as usual using `stack`. I recommend to first install
[ghcup](https://www.haskell.org/ghcup/), and then install the necessary tools
with the following commands:

```
ghcup install ghc --set 9.0.2
ghcup install stack
ghcup install hls
```

Installation of HLS is optional.

There are several system dependencies that are also required, such as LLVM-12.
Install these via your package manager of choice (homebrew, apt, etc.)


## Setup: Visual Studio Code

The repository contain the files necessary to build the project in a [Visual Studio Code Dev Container](https://code.visualstudio.com/docs/remote/containers).

- An Ubuntu container with all the required packages installed
- [VSCode Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell), with code highlights, hlint and mouse tooltip type inspection.
- [GitHub SSH Authentication](https://docs.github.com/en/authentication/connecting-to-github-with-ssh) and [GitHub SSH Signing](https://docs.github.com/en/authentication/managing-commit-signature-verification/signing-commits)
- [CUDA](https://docs.nvidia.com/cuda/wsl-user-guide/index.html) support to run [Accelerate](https://github.com/AccelerateHS/accelerate) on the host machine's NVIDIA GPU from the dev container.

### Requirements

- [VSCode](https://code.visualstudio.com/) With the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) installed
- [Docker](https://www.docker.com/get-started/)
- [Windows 11](https://www.microsoft.com/en-us/windows/windows-11?r=1) also requires [WSL2 with Ubuntu](https://docs.microsoft.com/en-us/windows/wsl/install)

### Installation

Aside from the GitHub SSH setup steps below (optional), the dev container works right out of the box.

Simply open this repository in VSCode with the remote containers extension installed, hit `ctrl + shift + p` and select `Rebuild and Reopen in Container`.

After around 10 to 15 minutes of building, the container should be ready to use.

> Note: after (re)building, when first clicking a Haskell file, the Haskell
> plugin has to compile the package. In vscode's notifications (footer bar),
> there will be an indication that it is still loading. This will resolve in
> around 10-15 minutes and only has to run after the first time of building the
> container.
>
> By default this will build using stack, since there is the stack.yaml file
> present. Thus it is recommended to also build and run the project using stack.

To check the status of compilation, head to `View > OutPut` and select the `Haskell` extension.

#### Setting up GitHub SSH Authentication & SSH Signing (optional)

The dev container supports GitHub SSH Authentication and GitHub SSH Signing.

In order to enable this, perform the steps below.

Setup WSL2 with Ubuntu as follows:

1. Create a `~/.ssh` folder with the following files.
    - Add github ssh private and public key to `~/.ssh/<KEYNAME>`, `~/.ssh/<KEYNAME>.pub`
    - Add config file to `~/.ssh/config` with the following contents
        ```
        Host github.com
            user git
            IdentityFile ~/.ssh/<KEYNAME>
        ```
2. Install packages `keychain`.
3. Create `~/.bash_profile` and add the following line:
    ```
    eval `keychain --eval --agents ssh github_30112021`.
    ```
4. Setup `~/.gitconfig` as follows
    ```
    [user]
        email = <EMAIL>
        name = <NAME>
        signingkey = <CONTENT OF PUBLIC KEY (e.g. ssh-ed25519 AAAAC3(...) user@example.com)>
    [commit]
        gpgsign = true
    [gpg]
        format = ssh
    [tag]
        gpgsign = true
    [core]
        editor = code
    ```

### Trouble Shooting

When in trouble, perhaps the instructions below might help.

#### Verify if GPU is correctly working

Run the command `nvidia-smi` in the dev container, the result should somewhat similar to the following:
```
+-----------------------------------------------------------------------------+
| NVIDIA-SMI 515.65.01    Driver Version: 516.94       CUDA Version: 11.7     |
|-------------------------------+----------------------+----------------------+
| GPU  Name        Persistence-M| Bus-Id        Disp.A | Volatile Uncorr. ECC |
| Fan  Temp  Perf  Pwr:Usage/Cap|         Memory-Usage | GPU-Util  Compute M. |
|                               |                      |               MIG M. |
|===============================+======================+======================|
|   0  NVIDIA GeForce ...  On   | 00000000:07:00.0  On |                  N/A |
|  0%   30C    P8    28W / 370W |   1512MiB / 10240MiB |      6%      Default |
|                               |                      |                  N/A |
+-------------------------------+----------------------+----------------------+

+-----------------------------------------------------------------------------+
| Processes:                                                                  |
|  GPU   GI   CI        PID   Type   Process name                  GPU Memory |
|        ID   ID                                                   Usage      |
|=============================================================================|
|  No running processes found                                                 |
+-----------------------------------------------------------------------------+
```

#### HLS Server crashed

If the Haskell plugin returns `The HLS server crashed 5 times in the past 3
minutes`, open up a terminal and type `stack build`. The plugin should run
correctly after reloading the window.

#### File Error Highlight: ensure that ghcide is compiled with the same GHC

If a file is highlighted with the error `Please ensure that ghcide is compiled
with the same GHC`, running `stack build` then closing and opening the file may
resolve the issue. Otherwise, reload the window.

#### Git Commit Error: unable to start editor 'editor'

When merging branches locally, if git throws `unable to start editor 'editor'`,
it can not find a suitable editor. To resolve this issue, ensure the git config
contains `core.editor = code`. For an example of a correct git config, see
[Setting up GitHub SSH Authentication & SSH Signing](#setting-up-github-ssh-authentication--ssh-signing).

