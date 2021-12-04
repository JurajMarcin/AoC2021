from typing import Iterable, Optional

Board = list[list[tuple[int, bool]]]

def load_numbers() -> Iterable[int]:
    return map(int, input().split(','))

def load_board() -> Optional[Board]:
    try:
        input()
    except EOFError:
        return None
    board = []
    for _ in range(5):
        board.append(list(
            map(lambda x: (int(x), False), input().strip().split())
        ))
    return board

def load_boards() -> Iterable[Board]:
    while (board := load_board()) is not None:
        yield board


def is_winning_board(board: Board) -> bool:
    for row in board:
        if all(map(lambda cell: cell[1], row)):
            return True
    for col in [ [ board[y][x] for y in range(5) ] for x in range(5) ]:
        if all(map(lambda cell: cell[1], col)):
            return True
    return False

def get_winning_boards(boards: list[Board]) -> list[Board]:
    winning_boards = []
    for board in boards:
        if is_winning_board(board):
            winning_boards.append(board)
    return winning_boards


def select_in_board(number: int, board: Board) -> Board:
    return [ [ (n, s or n == number) for n, s in row ] for row in board ]

def select_in_boards(number: int, boards: list[Board]) -> list:
    return list(map(lambda board: select_in_board(number, board), boards))


def print_board(board: Board) -> None:
    for row in board:
        for n, s in row:
            cell = str(n) + ('+' if s else ' ')
            print(f'{cell:>3}', end=' ')
        print()
    print()

def print_boards(boards: list[Board]) -> None:
    for board in boards:
        print_board(board)


def task1(numbers: list[int], boards: list[Board]) -> int:
    for number in numbers:
        boards = select_in_boards(number, boards)
        winning_boards = get_winning_boards(boards)
        if len(winning_boards) > 0:
            return sum(map(
                lambda row: sum(map(
                    lambda c: c[0],
                    filter(lambda c: not c[1], row)
                )),
                winning_boards[0]
            )) * number
    return -1


def task2(numbers: list[int], boards: list[Board]) -> int:
    last_to_win: Optional[Board] = None
    last_to_win_number = -1
    for number in numbers:
        boards = select_in_boards(number, boards)
        winning_boards = get_winning_boards(boards)
        if len(winning_boards) > 0:
            last_to_win = winning_boards[-1]
            last_to_win_number = number
        boards = list(filter(
            lambda board: board not in winning_boards, boards
        ))
    if last_to_win is not None:
        return sum(map(
            lambda row: sum(map(
                lambda c: c[0],
                filter(lambda c: not c[1], row)
            )),
            last_to_win
        )) * last_to_win_number
    return -1;


def main() -> None:
    numbers = list(load_numbers())
    boards = list(load_boards())
    print(task1(numbers, boards))
    print(task2(numbers, boards))


main()
