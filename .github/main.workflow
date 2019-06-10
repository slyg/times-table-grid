workflow "Deploy on Now" {
  on = "push"
  resolves = ["Alias"]
}

action "Deploy" {
  uses = "actions/zeit-now@master"
  secrets = [
    "ZEIT_TOKEN_TIMES_TABLES_GRID",
  ]
}

action "Alias" {
  uses = "actions/zeit-now@master"
  needs = "Deploy"
  args = "alias"
  secrets = [
    "ZEIT_TOKEN_TIMES_TABLES_GRID",
  ]
}