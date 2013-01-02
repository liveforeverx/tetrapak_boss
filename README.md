# Usage

Install tetrapak ( https://github.com/liveforeverx/tetrapak/ )

Install plugin

    $ git clone https://github.com/liveforeverx/tetrapak_boss/
    $ cd tetrapak_boss
    $ tetrapak build
    $ ensure, that tetrapak_boss is in folder, that are in ERL_LIBS configuration

By tetrapak

    $ by {plugin_scan, true} tetrapak will detect plugin automaticly for a project
    $ otherwise add in your <project>/tetrapak/config.ini 
        [tetrapak]
        plugins = ["boss"]

