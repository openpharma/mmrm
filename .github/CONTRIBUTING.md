<!-- markdownlint-disable-file -->
<!-- CONTRIBUTING.md needs to be generated from CONTRIBUTING.Rmd. Please edit that file -->

# Contributing

Thank you for your interest in contributing to this repo! Your
contribution is highly valued, and please go through this document to
help you contribute.

## Programming conventions

Please follow the programming conventions to ensure a consistent program
style across the package.

Generally we follow the [tidyverse style
guide](https://style.tidyverse.org/). Some specific conventions that
deviate from this are explained here.

### Functions

1.  Function names should be explicit and clear, separated by underscore
    (`snake_case`).
2.  Functions starting with `h_` are helper functions and they should
    not be exported.
3.  Functions should be well documented using `roxygen2` (even when they
    are not exported).
4.  Functions must be unit tested (even when they are not exported).

### Comments

- Comments should always follow sentence style.
- Comments should be as minimal as possible: Generally the code and
  variable names should be clear enough to not need any comments. Only
  use them when needed and explain the “Why” and not the “What”.

## Github conventions

When using github to collaborate, the following conventions are needed:

1.  Github issues is for issues, feature requests, bugs. Before creating
    a issue, please make sure this issue has not been reported yet.
    - If you are going to work on this issue, please assign yourself.
2.  Please create a branch in the `mmrm` repository, instead of creating
    forks, unless you are not yet a team member.
    - The name of a branch should be like
      `<issue_id>_<short_discription>`. Use an underscore to separate
      the description.
3.  Add changes to the branch and push it to github.
    - Please use clear commit messages.
    - Please keep your changes focused on the issue. If there are
      independent changes, please separate it into another PR linking
      to another issue.
4.  Please create a Pull Request when you think your code changes are
    ready:
    - Functions are well documented.
    - Functions have corresponding unit tests.
    - Changes pass all the github action checks.
    - The checklist in the corresponding issue is completed.
5.  Address all the comments you receive.
    - at least one approval is needed to merge.

## Contribution tips

### Development environment Set-up

The development this `mmrm` package is based on the latest R version and
C++ compilers. The package dependencies are the most recent versions
from CRAN. We recommend that your working environment is set up in the
same way. Additionally, there are some tools we recommend you to
install:

1.  [`RTools`](https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html)
    if you work on a Windows operating system. Alternatively you can use
    [`docker`](https://www.docker.com/) to separate the operating system
    and the development system.
2.  [`GitKraken`](https://www.gitkraken.com/) is a very useful user
    interface for git including visualization of git commit graphs, file
    history, etc.
3.  [`lintr`](https://openpharma.github.io/mmrm/main/articles/package_structure.html#lintr)
    will allow you to perform static code analysis.
4.  [`pre-commit`](https://openpharma.github.io/mmrm/main/articles/package_structure.html#pre-commit-config.yaml)
    is a Python module that allow you to identify issues before you
    commit locally.

### Issue labels

The issues are categorized with several labels:

| Label name         | Description                                                                                                                        |
| ------------------ | ---------------------------------------------------------------------------------------------------------------------------------- |
| `SPx`              | `SP` (story points) indicate complexity, and the larger the subsequent number, the more time consuming the issue is expected to be |
| `priority`         | Issues with this label should be completed with higher priority                                                                    |
| `good first issue` | Good choices for new team members                                                                                                  |
| `blocked`          | Blocked by other issues                                                                                                            |
| `bug`              | Something isn’t working                                                                                                            |
| `devops`           | Development and Operation                                                                                                          |
| `discussion`       | Disccusion needed                                                                                                                  |
| `documentation`    | Improvement of documentation is needed                                                                                             |
| `duplicate`        | The issue already exists                                                                                                           |
| `enhancement`      | New feature or request                                                                                                             |
| `help wanted`      | Extra attention is needed                                                                                                          |
| `invalid`          | This doesn’t seem right                                                                                                            |
| `question`         | Further information needed                                                                                                         |

Please choose an issue based on your interest, issue complexity, and
priority.

### Add new unit tests

To add a new unit test, you need to first identify the test scope. Does
the test fit in the scope of existing tests? If so, please modify the
existing test files under `tests/testthat/` folder or `src/` folder,
depending on whether the code to be tested is R or C++. Otherwise please
create a new test file, with a name prefix of “test-”.

In each test case, use the following structure:

```r
test_that("<function_name> do something as explained", {
  # test body
  expect_identical(1, 1)
})
```

The purpose of the test should be clearly stated first.

In the test body part, conduct the tests, e.g. use `expect_identical` to
check consistency, `expect_error` to catch error messages, etc. The test
body should not follow the same implementation logic as the package did,
otherwise we may miss mistakes in implementation.

### Add integration tests

Integration tests compare the results of SAS and R and assures the
quality of our code. To add an integration test, you need to do the
following:

1.  Use SAS to run an appropriate mmrm models with `proc mixed`, using
    `fev_data`.
2.  Save the results in `txt` format in the `design/SAS/` folder.
3.  Decide the key outputs that are needed for comparison.
4.  Add a unit test verifying that the R implementation of the same
    model has the same results (conversion may be needed).

### Get started with C++

If you have no experience with C++, it is totally fine: `TMB` has
provided us with many high-level functionalities that is very similar to
`R`. Here we only list the most important things that you need to go
through before you begin C++ programming.

1.  Semicolons. C++ use semicolons to terminate a statement. In R, we
    can use semicolons or line breaks to do so, but in C++, we need both
    semicolons and line breaks.
2.  Types. C++ is a strong type programming language and all objects
    need to be declared with type. Sometimes you can combine the
    declaration and definition.
    1.  `int i = 1;` This works, as we declare `i` as `int` and define
        it to be 1.
    2.  `i = 1;` This fails, because `i` is not declared yet.
    3.  `int i; i = 1;` This works, because `i` is declared and then
        defined.
    4.  `int i = 1, j = 2;` This works, because `i` and `j` are both
        `int`.
3.  Object scope. In R, objects declared inside control flows will exist
    outside of that enclosing brackets of the flow, while in C++,
    objects created within control flows will be terminated.
    1.  in R, `if (TRUE) { a = '123' }; print(a)` is legal.
    2.  in C++,
        `if (1) {string a = '123'}; std::stdout << a << std::endl;` is
        illegal, because object `a` is terminated already.
    3.  in C++,
        `string a; if (1) {a = '123'}; std::stdout << a << std::endl;`
        is legal, `a` is declared prior to if statement.
4.  Polymorphism. Unlike in R, where function arguments have no
    explicitly defined types, in C++ the type of each argument of a
    function must be pre-specified.
    1.  `Template` is a special function that works with generic
        argument type. We could imagine a single function that could
        work on arguments of arbitrary type, and `template` functions
        make this possible through separation of function logic from the
        argument declaration. In this way we can use `template`
        functions and avoid the need to replicate the whole code for
        each type.

With these points in mind, you are about ready to go.

### Get started with TMB

In `mmrm` we are not including any latent variables and so the Laplace
approximation aspect of `TMB` is not used. We only use the automatic
differentiation part of `TMB`. For detailed documentation of `TMB`,
visit the [TMB
reference](https://kaskr.github.io/adcomp/Introduction.html).

One important feature of `TMB` are the R style matrix/array
calculations. This is important because we mainly use this part to
conduct our calculations. See
[matrix_arrays.cpp](https://kaskr.github.io/adcomp/matrix_arrays_8cpp-example.html)
for examples.

### Add a new covariance structure

To add a new covariance structure, you need to do the following:

1.  Understand the covariance structure and add appropriate
    documentation in [covariance
    structure](https://openpharma.github.io/mmrm/main/articles/covariance.html)
    and you can create a draft pull request to invite discussion from
    other team members.
2.  Implement the covariance structure on the C++ side and the
    corresponding R interface.
3.  Add unit tests to make sure the new covariance structure is working
    as expected.
4.  Add integration tests under `design/SAS/` folder to make sure SAS
    and R produce similar results (within tolerance).

### Add additional data

To add additional data to `mmrm`, please follow the next steps:

1.  Make sure the data is needed.
2.  Document the data in `R/data.R`.
3.  Use `save` to create an `rda` data **only** containing this dataset.
    Here the function `usethis::use_data` can also be helpful.

## Communications within team

There are several communication channels, please use appropriate ones.

### GitHub

GitHub issues and pull requests are where implementations are discussed
and reviewed. Feature requests, bugs, enhancements, technical
implementations can be discussed here. When you have ideas that needs to
be documented, it is better to have them in github.

### Slack

Slack is a messaging tool and we have the `mmrm` channel under the
`rinpharma` space. You can put anything in the slack channel, e.g., you
have completed a issue and are waiting for review, or you have some
questions and don’t want to wait until the next stand-up meeting.

To join the slack channel, please make sure you have a slack account,
and send the email address to any team member.

### Stand-up meetings

We have stand-up meetings currently three times a week. The purpose of
stand-up meetings is for team members to discuss the current work
progress and raise any issue/questions for team discussion if needed.

You don’t need to join all three meetings.
