/* pai-sandbox: run a command as the sandbox user (bead v3-r2j.5).
 *
 * Installed setuid root in the agent image. The agent's shell commands run
 * through it, so they run as uid/gid 1001 with no supplementary groups, and
 * cannot read the agent's own directory (keyring, secrets, ledger), which is
 * owned by the agent's user and mode 700. It can only ever become 1001: no
 * argument chooses the user, and it gives up root for good before exec.
 */
#define _GNU_SOURCE
#include <grp.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>

#define SANDBOX_ID 1001

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: pai-sandbox command [args...]\n");
        return 2;
    }
    if (setgroups(0, NULL) != 0 || setresgid(SANDBOX_ID, SANDBOX_ID, SANDBOX_ID) != 0 ||
        setresuid(SANDBOX_ID, SANDBOX_ID, SANDBOX_ID) != 0) {
        perror("pai-sandbox: could not become the sandbox user");
        return 126;
    }
    /* Root must be gone for good: if it can be regained, refuse to run. */
    if (setuid(0) == 0 || geteuid() != SANDBOX_ID || getegid() != SANDBOX_ID) {
        fprintf(stderr, "pai-sandbox: privileges were not dropped\n");
        return 126;
    }
    umask(002);   /* files it makes stay readable and writable by the agent's group */
    execvp(argv[1], argv + 1);
    perror("pai-sandbox: exec");
    return 127;
}
