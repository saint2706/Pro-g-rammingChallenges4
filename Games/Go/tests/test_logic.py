import pytest

from Games.Go.logic import GoGame, IllegalMove


def test_capture_single_stone():
    game = GoGame(9)
    game.play(0, 1)  # B
    game.play(1, 1)  # W
    game.play(1, 0)  # B
    game.play(4, 4)  # W
    game.play(1, 2)  # B
    game.play(4, 5)  # W
    game.play(2, 1)  # B - captures the white stone at (1,1)

    assert game.board[1][1] is None
    assert game.captures["B"] == 1


def test_suicide_detection_blocks_illegal_move():
    game = GoGame(9)
    for point in [(0, 1), (1, 0), (1, 2), (2, 1)]:
        game.board[point[0]][point[1]] = "W"
    game.to_move = "B"
    game._position_history = [game._board_key(game.board)]
    game._snapshots = [
        (game._copy_board(game.board), game.to_move, game.captures.copy())
    ]

    with pytest.raises(IllegalMove):
        game.play(1, 1)


def test_ko_rule_prevents_immediate_retake():
    game = GoGame(9)
    # Ko shape discovered programmatically: see logic brute force helper.
    setup = {
        (0, 1): "B",
        (1, 0): "B",
        (1, 1): "W",
        (2, 0): "W",
    }
    for (r, c), color in setup.items():
        game.board[r][c] = color
    game.to_move = "W"
    game._position_history = [game._board_key(game.board)]
    game._snapshots = [
        (game._copy_board(game.board), game.to_move, game.captures.copy())
    ]

    game.play(0, 0)  # White captures the black stone at (1,0)
    assert game.board[1][0] is None

    with pytest.raises(IllegalMove):
        game.play(1, 0)


def test_scoring_counts_area_and_territory():
    game = GoGame(9)
    black_ring = {
        (3, 3),
        (3, 4),
        (3, 5),
        (4, 3),
        (4, 5),
        (5, 3),
        (5, 4),
        (5, 5),
    }
    white_cross = {(6, 5), (5, 6), (6, 7), (7, 6)}
    for r, c in black_ring:
        game.board[r][c] = "B"
    for r, c in white_cross:
        game.board[r][c] = "W"
    game.captures["B"] = 2
    game.captures["W"] = 1
    game._position_history = [game._board_key(game.board)]
    game._snapshots = [
        (game._copy_board(game.board), game.to_move, game.captures.copy())
    ]

    scores = game.score()
    assert scores["territory"]["B"] == 3  # 2 captures + 1 surrounded point
    assert scores["area"]["B"] == 11  # 2 captures + 8 stones + 1 territory
    assert scores["territory"]["W"] == 2  # 1 capture + 1 surrounded point
    assert scores["area"]["W"] == 6  # 1 capture + 4 stones + 1 territory


def test_undo_reverts_board_and_captures():
    game = GoGame(9)
    game.play(0, 0)
    game.play(0, 1)
    assert game.to_move == "B"

    undone = game.undo()
    assert undone is True
    assert game.to_move == "W"
    assert game.board[0][1] is None
    assert len(game.moves) == 1

    # Undo initial move
    game.undo()
    assert game.to_move == "B"
    assert not game.moves
