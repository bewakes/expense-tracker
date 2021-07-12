# Expenses Tracker
A brand new version of Expense Tracker.

- [Features](#features)
- [TODO](#todo)
- [Database Setup](#database-setup)
- [Haskell Setup](#haskell-setup)
- [Development](#development)
- [Tests](#tests)

<small><i><a href='http://ecotrust-canada.github.io/markdown-toc/'>Table of contents generated with markdown-toc</a></i></small>


## Features
- Beautiful dashboard showing Daily and category wise bar charts along with a category based pie chart. 
- You can add multiple groups to track expenses for, e.g. Personal(default), Home, Mt.Everest Hike group, Office, you name it.
- Upon signup, you'll have your "Personal" group which includes just you.
- Provides default 6 categories for expenses: `Groccery`, `Utility`, `Medicine`, `Fruits`, `Stationary` and `Miscellaneous`. See To-do below for coming features.
- You can add members to your groups.

## TODO
- [ ] Edit/Remove expenses.
- [ ] Add multiple categories.
- [ ] Notifications when members change expenses in group.

## Database Setup

Run `docker-compose up`. Note that it should have `expense_tracker` database.

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Tests
No tests so far. :(
