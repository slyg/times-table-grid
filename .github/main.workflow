workflow "Deploy on Now" {
  on = "push"
  resolves = ["Deploy"]
}

action "Deploy" {
  uses = "actions/zeit-now@master"
  args = "alias"
  secrets = [
    "ZEIT_TOKEN_TIMES_TABLES_GRID"
  ]
}
