# stuff to add at end of ~/.bash_logout
if ((agent_started)); then
  echo "Killing ssh agent"
  ssh-agent -k
fi
