

# mllrnrs NEWS

## Unreleased (2025-03-05)

#### Docs

-   first try to fix issues with vignettes that occur only on macosx
    cran runners
    ([836b023](https://github.com/kapsner/mllrnrs/tree/836b023f0bfe26cc937faa55df9bcea3b4837527))

#### Other changes

-   providing nthread with parameters now
    ([a85fd0e](https://github.com/kapsner/mllrnrs/tree/a85fd0eb4ba1b2c8db65fc4d1dc1d25dcd2e3974))
-   adresses notes on cran-checks
    ([375011f](https://github.com/kapsner/mllrnrs/tree/375011f202925234d6b6e57246e7f31b57c82f35))

Full set of changes:
[`v0.0.4...a85fd0e`](https://github.com/kapsner/mllrnrs/compare/v0.0.4...a85fd0e)

## v0.0.4 (2024-07-05)

#### New features

-   weights support also for ranger
    ([935e0dc](https://github.com/kapsner/mllrnrs/tree/935e0dcbd24ebddd05ad6caea96af8d57e9bfe35))
-   added weights support for lightgbm
    ([c72161c](https://github.com/kapsner/mllrnrs/tree/c72161c5e86ad8416c2cb3e6051a060253dd3ebc))
-   added option to add weights for xgboost multiclass
    ([694ca7e](https://github.com/kapsner/mllrnrs/tree/694ca7e56df25ee94d3937bc6b8ab1015de619db))

#### Tests

-   added weighted classification to unit tests
    ([ca30135](https://github.com/kapsner/mllrnrs/tree/ca30135fabf519ad82f08e9e38a289efb5b06e13))

#### CI

-   fix gha
    ([f57d08d](https://github.com/kapsner/mllrnrs/tree/f57d08df237313e85caa307d3c7b68aa8879185c))

#### Other changes

-   fixed indentation
    ([d5e4306](https://github.com/kapsner/mllrnrs/tree/d5e430689e1e81ba15f5c6803028d18993247f3b))
-   updated version for cran-release
    ([2ff35e2](https://github.com/kapsner/mllrnrs/tree/2ff35e2c237c03a6e566e269744c5b15df973c7e))
-   switch vignetteengine to quarto
    ([b70f3d6](https://github.com/kapsner/mllrnrs/tree/b70f3d678751b67513d1404062362b651d5e806a))
-   added automated gen of readme
    ([dbbcde5](https://github.com/kapsner/mllrnrs/tree/dbbcde53cbc4ba0f3430360111b50c02a2dec33b))
-   add weight support also for glmnet
    ([1674811](https://github.com/kapsner/mllrnrs/tree/1674811286f31ef4bfae5f351686396e5f1845ed))
-   updated dev-version
    ([4b1625c](https://github.com/kapsner/mllrnrs/tree/4b1625cc389701ce114b44ee8565f0b147362483))
-   updated news.md
    ([323f9cb](https://github.com/kapsner/mllrnrs/tree/323f9cb0dc0e38213154669dccff7a700f5071c2))

Full set of changes:
[`v0.0.3...v0.0.4`](https://github.com/kapsner/mllrnrs/compare/v0.0.3...v0.0.4)

## v0.0.3 (2024-03-07)

#### New features

-   preparing v0.0.3
    ([a6b04fc](https://github.com/kapsner/mllrnrs/tree/a6b04fc8c110864418bf201d2d77e0c30218c4ac))

#### Other changes

-   added cran installation note
    ([a1aa854](https://github.com/kapsner/mllrnrs/tree/a1aa854b479d78e67a7eb76b71b7d5a4500171fd))

Full set of changes:
[`v0.0.2...v0.0.3`](https://github.com/kapsner/mllrnrs/compare/v0.0.2...v0.0.3)

## v0.0.2 (2023-07-18)

#### Bug fixes

-   also removed reshape argument for prediction
    ([09cde9d](https://github.com/kapsner/mllrnrs/tree/09cde9d8eb6e38310d405ce4f669eb6fb64a1b37))
-   temporary fix for compatibility with lightgbm
    ([7a9ef30](https://github.com/kapsner/mllrnrs/tree/7a9ef307926c07ca6f2b33c2d93f4431d2a0a8c8))
-   adaptions to cat_vars
    ([e9f0eff](https://github.com/kapsner/mllrnrs/tree/e9f0effbc2b1274f8f32a86c12b4a68ece9163de))

#### Tests

-   reducing runtime of unit-tests
    ([e8b42e7](https://github.com/kapsner/mllrnrs/tree/e8b42e7bed2392900d1495e666ce0363cfe0a3e4))

#### CI

-   moving to code_step
    ([2e4d11a](https://github.com/kapsner/mllrnrs/tree/2e4d11a96f5f91f62e05772f77237ac384f7e130))
-   update coverage installing packages
    ([a2aec56](https://github.com/kapsner/mllrnrs/tree/a2aec569a52d66c2ba9feb2c6ae825c1afb3be92))
-   shorter ci runtimes
    ([2382057](https://github.com/kapsner/mllrnrs/tree/2382057f7827370c7e4f7a5cb575fe0d663f026a))
-   explicitly installing suggests
    ([46a657c](https://github.com/kapsner/mllrnrs/tree/46a657c03da03110917dcc3654d7b9967af5d97a))
-   updated tic.r
    ([de4feaf](https://github.com/kapsner/mllrnrs/tree/de4feaf376eb6d0af75cce19d0bc4cd50b223275))

#### Docs

-   fixing issues for cran submission
    ([15cbeda](https://github.com/kapsner/mllrnrs/tree/15cbedab83d2676abd08716209e92ba606bfb751))
-   making vignettes static 2
    ([0dd73e6](https://github.com/kapsner/mllrnrs/tree/0dd73e646ea7270eecd1c9747d643ede2e1b5e5a))
-   working on making vignettes static
    ([e68cbdf](https://github.com/kapsner/mllrnrs/tree/e68cbdf55304832c6843e5c9a427f527a5250ffa))

#### Other changes

-   updated news.md and description
    ([9d23524](https://github.com/kapsner/mllrnrs/tree/9d23524cc0422f381be74ac854c128ec69bfd468))
-   updated news.md and description
    ([47aeca9](https://github.com/kapsner/mllrnrs/tree/47aeca9df9197de14eb8c4b7a37a7579e3f55b33))
-   updated cran-comments
    ([9be58ff](https://github.com/kapsner/mllrnrs/tree/9be58ffe0b30afa956dc8ad9732e2a7fdac65ab5))
-   updated news.md
    ([000073f](https://github.com/kapsner/mllrnrs/tree/000073f2cddbe5671bc448e8d9d6b0e0bc2e3f99))
-   updated description and news.md
    ([2db2e9b](https://github.com/kapsner/mllrnrs/tree/2db2e9bcf3525b872e5dce57ec00e517f6f3be5e))
-   appended rbuildignore
    ([af0dc5f](https://github.com/kapsner/mllrnrs/tree/af0dc5ffbf064274e313d2115a5dbcdd41b97581))
-   preparing initial cran submission
    ([39b1e94](https://github.com/kapsner/mllrnrs/tree/39b1e94d5a3989b0189ce10471dd9d964a7ed549))
-   adding parbayesianoptimization and mlr3measures to suggests
    ([895e483](https://github.com/kapsner/mllrnrs/tree/895e4830a5ff659883f37265b304be718973ca31))
-   preparing initial cran submission
    ([6550826](https://github.com/kapsner/mllrnrs/tree/6550826ec135e19882c746b5ec20705a5f31e15c))
-   fixed typo
    ([eaa42a2](https://github.com/kapsner/mllrnrs/tree/eaa42a2bddf0b3c263c72ec22fc6f92132d24ee6))
-   updated ranger tests
    ([1b9d9dd](https://github.com/kapsner/mllrnrs/tree/1b9d9ddaf89b50e7b19f6d701e1e3b6826f05b0a))
-   fixed typo
    ([df54b3d](https://github.com/kapsner/mllrnrs/tree/df54b3d69207dba0893a4dedaa97f0f843178390))
-   updated wiki.r
    ([af08974](https://github.com/kapsner/mllrnrs/tree/af0897471d09d71b29a10218030ad573a1b303c0))

Full set of changes:
[`v0.0.1...v0.0.2`](https://github.com/kapsner/mllrnrs/compare/v0.0.1...v0.0.2)

## v0.0.1 (2022-11-13)

#### New features

-   transferred survival learners to new package
    ([224b53d](https://github.com/kapsner/mllrnrs/tree/224b53de29300d070ed9b91aac87915732f3a350))
-   added survivalsvm learner
    ([18e35f2](https://github.com/kapsner/mllrnrs/tree/18e35f2d70d90791e0a1b775841fd0c5bafc35eb))
-   added xgboost aft survival learner
    ([c079e56](https://github.com/kapsner/mllrnrs/tree/c079e56eecc4461cdb5334bca60830024c84883b))
-   finalized glmnet learner0
    ([f27e4e6](https://github.com/kapsner/mllrnrs/tree/f27e4e6b5cbdba7616acae247628a5560ccede9c))
-   working on glmnet learner
    ([101b15b](https://github.com/kapsner/mllrnrs/tree/101b15bac5795ee285a36e7c9b312c0443062499))
-   working on adding glmnet learner (wip)
    ([2adaf40](https://github.com/kapsner/mllrnrs/tree/2adaf405a30679fb49233f5f4a42279e0d688541))
-   working on adding rpart for survival data
    ([7e16ec8](https://github.com/kapsner/mllrnrs/tree/7e16ec8219b6b9aee42b0b12c8d5e08e752ea627))
-   added learnerranger
    ([4966ff3](https://github.com/kapsner/mllrnrs/tree/4966ff3d2320de6868e6ce99fa5e3cb12f41be36))
-   added lgb
    ([3b777b4](https://github.com/kapsner/mllrnrs/tree/3b777b4d2f7ecaf7d90c80bbc7a3a9a7f2c3bfe6))
-   xgboost multiclass
    ([e72ce46](https://github.com/kapsner/mllrnrs/tree/e72ce46d870d73bf5aa43d12c7270074f8697700))
-   working on adding xgboost
    ([42ef329](https://github.com/kapsner/mllrnrs/tree/42ef329443ef56b95ea5d5e632e1d733f91b1dab))

#### Bug fixes

-   back to arithmetic mean of nrounds for xgboost aft models
    ([9771479](https://github.com/kapsner/mllrnrs/tree/9771479c6ba609f5b950d39cdc2154ede57c6a0c))
-   fixed issues with rpart survival learner
    ([9c64352](https://github.com/kapsner/mllrnrs/tree/9c643525dea8863a40be5792fb9e8dac79b0b616))
-   fixed ranger learner to work with all tasks
    ([eec0a4f](https://github.com/kapsner/mllrnrs/tree/eec0a4f5dab8d00a24c10046ff8c7a02e5e3cdc7))
-   adaptions to upstream changes
    ([e82693c](https://github.com/kapsner/mllrnrs/tree/e82693c3725313f13f57359d9656e595b3b22564))
-   working on functioning of code
    ([3705344](https://github.com/kapsner/mllrnrs/tree/3705344129e61879259fb4e025d8af3db03025e1))

#### Refactorings

-   changed order of list to be updated
    ([066eb5a](https://github.com/kapsner/mllrnrs/tree/066eb5a05408e92025c570b58def13c29ed25bab))
-   to metric_types_helper from mlexperiments
    ([1099f1a](https://github.com/kapsner/mllrnrs/tree/1099f1aac600c2cac41a789ca5bdfcc91f2f802b))
-   code adaptions to upstream changes
    ([c5363fc](https://github.com/kapsner/mllrnrs/tree/c5363fc2be191343ef5ebfd3363a535e0ea5e458))

#### CI

-   shortended ci to only nested cv testings
    ([981b6f2](https://github.com/kapsner/mllrnrs/tree/981b6f2015c1afc3622ddc407874da61c04fc75a))
-   fixed ci
    ([832c3a3](https://github.com/kapsner/mllrnrs/tree/832c3a3f1ad1e3703b9e00600818fcf45b1a7245))
-   reduced messages from lgb training
    ([1d0a7c7](https://github.com/kapsner/mllrnrs/tree/1d0a7c7881065df79274579809e6e307bff40f4b))
-   removed building of vignettes
    ([8ea1931](https://github.com/kapsner/mllrnrs/tree/8ea193198e556056952db2dbf22fa94e4e16c695))
-   working on fixing tests
    ([dccc08a](https://github.com/kapsner/mllrnrs/tree/dccc08ab68d15d2e4a49c23c29936f746ba13cab))
-   finished unit tests for ranger
    ([84fa3aa](https://github.com/kapsner/mllrnrs/tree/84fa3aae461ee1479e0d173ad69c75454e1193dc))
-   adapted tests to upstream changes
    ([66d7534](https://github.com/kapsner/mllrnrs/tree/66d75346960ed9df07dbf88743bf9f6e9952a777))
-   fixed linting errors
    ([e6d411e](https://github.com/kapsner/mllrnrs/tree/e6d411edcc1f280759f29b157fe476162a74c94a))
-   refactored tests; finalized xgboost:binary
    ([1e6993e](https://github.com/kapsner/mllrnrs/tree/1e6993ef7e7cb9569a9a5dbef60be66d83cbdfa0))
-   fixed linting errors
    ([6fcf776](https://github.com/kapsner/mllrnrs/tree/6fcf776dde9ba7c6b2b6cce47aa864c9d1c9fd00))
-   added package name to functions
    ([9622cfe](https://github.com/kapsner/mllrnrs/tree/9622cfeac556a3e8341078e138974a42b81b8c90))

#### Docs

-   added and updated vignettes
    ([b356633](https://github.com/kapsner/mllrnrs/tree/b3566339875dc200b850688564e102163260feba))
-   added ranger vignettes
    ([9da7bb9](https://github.com/kapsner/mllrnrs/tree/9da7bb90f7467f77f187406a1fbc11f8c1b88042))
-   working on vignettes
    ([7af00e0](https://github.com/kapsner/mllrnrs/tree/7af00e0af8e6f282e7cf6dd709defa7d5a9448dc))
-   added glmnet vignette
    ([da43c59](https://github.com/kapsner/mllrnrs/tree/da43c5902d71f6f4e864f26c626c3900aed8eada))
-   started working on vignettes
    ([8cefc39](https://github.com/kapsner/mllrnrs/tree/8cefc399987adee3302783a4561755bd0499d208))
-   updated readme
    ([b14b176](https://github.com/kapsner/mllrnrs/tree/b14b17683dc8a54eb843b54aa1f562ffc7354285))
-   updated learner table
    ([29298aa](https://github.com/kapsner/mllrnrs/tree/29298aac830529dbabcb1d6f0351c9c2fa14d90d))
-   updated details
    ([296a265](https://github.com/kapsner/mllrnrs/tree/296a2650121722f2e269b4496961b9d762136cbd))
-   updated details
    ([ad6694b](https://github.com/kapsner/mllrnrs/tree/ad6694b39ae7168d4e4cb80c972596216f747473))
-   updated details
    ([23d1ba0](https://github.com/kapsner/mllrnrs/tree/23d1ba0872d5c6541756696a27db3a5b376a89ab))
-   updated details
    ([d8be190](https://github.com/kapsner/mllrnrs/tree/d8be19038f50d4dd98d2ced08e92a2edffd4e72f))
-   updated details
    ([ef28917](https://github.com/kapsner/mllrnrs/tree/ef289171cd3072b882ce874a2e974d2f3c87226c))
-   updated details
    ([1b0fc17](https://github.com/kapsner/mllrnrs/tree/1b0fc17bba5379df75608db7f92d9e1b95281396))
-   updated readme
    ([e8cfcb3](https://github.com/kapsner/mllrnrs/tree/e8cfcb36aeb2ff29d622179a64b2a1f65db32e78))
-   updated readme and added backlog
    ([fcd1de7](https://github.com/kapsner/mllrnrs/tree/fcd1de7f7e800367ea6f99e93342c8c432907139))
-   working on documentation
    ([36dc09e](https://github.com/kapsner/mllrnrs/tree/36dc09ed9a3964172320151ad9351df764de9c6d))
-   started working on documentation
    ([beaba4b](https://github.com/kapsner/mllrnrs/tree/beaba4bf2bf78dd39c1f0b4114ee98e8fc1a718e))

#### Other changes

-   renamed vignettes filenmaes
    ([9624681](https://github.com/kapsner/mllrnrs/tree/96246818c6a28b68a9f276fe6b89b0ad694dfabc))
-   updated description and news.md
    ([26110f2](https://github.com/kapsner/mllrnrs/tree/26110f2560889a8d9b6f9f4abb60bb4439c89db8))
-   updated docs
    ([fc5857c](https://github.com/kapsner/mllrnrs/tree/fc5857c4e0ce69a9b660e6f881b224af92367267))
-   fixed linting errors
    ([8fb039c](https://github.com/kapsner/mllrnrs/tree/8fb039cef6463848fc3d81aad6205b5fc3c415af))
-   updated description and news.md
    ([00c72da](https://github.com/kapsner/mllrnrs/tree/00c72da72cc1cba4b2d67c8f6d7de234f7baa181))
-   updated message from learner
    ([4e9f2c5](https://github.com/kapsner/mllrnrs/tree/4e9f2c5c9b005ad09a83010581b2f90b1b62875c))
-   updated description
    ([6d169c5](https://github.com/kapsner/mllrnrs/tree/6d169c5da56da61146ac9181313a3db813ccfb4b))
-   updated readme, added lifecycle badge
    ([5dd1d33](https://github.com/kapsner/mllrnrs/tree/5dd1d330f44fc4a25e4c6de63bd288d57576b846))
-   updated news.md
    ([03bbf65](https://github.com/kapsner/mllrnrs/tree/03bbf65aff08304f9bcaa6f22ea554a5bed39358))
-   updated todos
    ([1f4a6b5](https://github.com/kapsner/mllrnrs/tree/1f4a6b52c1f9a64eb7143e99843618329182ca08))

Full set of changes:
[`ceea1ce...v0.0.1`](https://github.com/kapsner/mllrnrs/compare/ceea1ce...v0.0.1)
