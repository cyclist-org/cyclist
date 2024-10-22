cd /home/cyclist/src/generic/test
export PKG_CONFIG_PATH="/opt/spot/accset-32/lib/pkgconfig" && pkg-config --cflags libspot libbddx
export LD_LIBRARY_PATH="/opt/spot/accset-32/lib"
make -s GRAPHS_DATABASE_PATH=/home/database run-get-graphs-stats > /home/stats.csv