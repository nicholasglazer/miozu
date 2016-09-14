#!/bin/bash
# This command will break if you rename it to
# something containing "cmus".

function env-session()
{

SESSION_NAME="env"


tmux has-session -t ${SESSION_NAME}

# Create the session
tmux new-session -s ${SESSION_NAME} -n cmus -d
tmux send-keys -t ${SESSION_NAME} 'cmus' C-m
tmux send-keys -t ${SESSION_NAME} 'cmus-remote -u' C-m
tmux split-window -v -t ${SESSION_NAME}
tmux send-keys -t ${SESSION_NAME} 'cava' C-m

tmux select-window -t ${SESSION_NAME}:1.2
tmux attach -t ${SESSION_NAME}
}


if ! pgrep cmus ; then
    env-session;
else
    killall -9 cmus cava;
    env-session
fi
