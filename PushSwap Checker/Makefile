SRC	=	Main.hs	\
	Commands.hs

NAME 	=	pushswap_checker

OBJ	=	$(SRC:.hs=.o)

all:
	ghc -o $(NAME) $(SRC)

clean	:
	rm -f $(OBJ) *.hi

fclean:	clean
	rm -f $(NAME)

re:	fclean all

.PHONY:	all clean fclean re