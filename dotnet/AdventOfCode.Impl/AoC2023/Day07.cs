﻿using System.Diagnostics;
using AdventOfCode.Impl.Data;

namespace AdventOfCode.Impl.AoC2023;

public static class Day07
{
    public static AoCResult<int, int> Solve(
        string input = Input)
    {
        var camelCards = CamelCards.Parse(input);

        return AoCResult.Create(
            camelCards.TotalWinningsSimple(),
            camelCards.TotalWinningsWithJokers());
    }

    private sealed record CamelCards(
        List<Hand> Hands)
    {
        public int TotalWinningsSimple()
            => SumWithComparer(new SimpleComparer());

        public int TotalWinningsWithJokers()
            => SumWithComparer(new JokerComparer());

        private int SumWithComparer(
            IComparer<Hand>? comparer)
            => Hands
                .Order(comparer)
                .Select((hand, idx) => (hand, Rank: idx + 1))
                .Select(t => t.hand.Bid * t.Rank)
                .Sum();

        public static CamelCards Parse(
            string input)
            => new(
                input
                    .SplitWithNewline()
                    .Select(Hand.Parse)
                    .ToList());
    }

    private sealed class SimpleComparer
        : IComparer<Hand>
    {
        public int Compare(
            Hand? x,
            Hand? y)
        {
            if (ReferenceEquals(x, y))
            {
                return 0;
            }

            if (ReferenceEquals(null, y))
            {
                return 1;
            }

            if (ReferenceEquals(null, x))
            {
                return -1;
            }

            var strengthCompared = x.StrengthSimple().CompareTo(y.StrengthSimple());

            if (strengthCompared == 0)
            {
                return CompareListsElementwise(
                    x.Cards,
                    y.Cards);
            }

            return strengthCompared;
        }

        private static int CompareListsElementwise(
            List<Card> left,
            List<Card> right)
        {
            for (var i = 0; i < left.Count; i++)
            {
                var compared = left[i].CompareTo(right[i]);

                if (compared != 0)
                {
                    return compared;
                }
            }

            return 0;
        }
    }

    private sealed class JokerComparer
        : IComparer<Hand>
    {
        public int Compare(
            Hand? x,
            Hand? y)
        {
            if (ReferenceEquals(x, y))
            {
                return 0;
            }

            if (ReferenceEquals(null, y))
            {
                return 1;
            }

            if (ReferenceEquals(null, x))
            {
                return -1;
            }

            var strengthCompared = Strength(x).CompareTo(Strength(y));

            if (strengthCompared == 0)
            {
                return CompareListsElementwise(
                    x.Cards,
                    y.Cards);
            }

            return strengthCompared;
        }

        private static HandStrength Strength(
            Hand hand)
        {
            var jokers = hand.Cards.Count(static c => c == Card.J);

            if (jokers == 0)
            {
                return hand.StrengthSimple();
            }

            var grouped = hand.Cards
                .Where(static c => c != Card.J)
                .GroupBy(static c => c)
                .Select(static g => (g.Key, Count: g.Count()))
                .OrderByDescending(static t => t.Count)
                .ThenByDescending(static t => t.Key)
                .ToList();

            return (jokers, grouped) switch
            {
                (5, []) => HandStrength.FiveOfAKind,
                // Upgrade four of a kind
                (1, [(_, 4)]) => HandStrength.FiveOfAKind,
                // Upgrade three of a kind
                (2, [(_, 3)]) => HandStrength.FiveOfAKind,
                (1, [(_, 3), ..]) => HandStrength.FourOfAKind,
                // Upgrade two pair
                (1, [(_, 2), (_, 2)]) => HandStrength.FullHouse,
                // Upgrade one pair
                (2, [(_, 2), ..]) => HandStrength.FourOfAKind,
                (1, [(_, 2), (_, 1), (_, 1)]) => HandStrength.ThreeOfAKind,
                // Upgrade high card
                (4, [(_, 1), ..]) => HandStrength.FiveOfAKind,
                (3, [(_, 1), ..]) => HandStrength.FourOfAKind,
                (2, [(_, 1), ..]) => HandStrength.ThreeOfAKind,
                (1, [(_, 1), ..]) => HandStrength.OnePair,
                _ => throw new UnreachableException()
            };
        }

        private static int CompareListsElementwise(
            List<Card> left,
            List<Card> right)
        {
            for (var i = 0; i < left.Count; i++)
            {
                var compared = MapToWeakJoker(left[i])
                    .CompareTo(MapToWeakJoker(right[i]));

                if (compared != 0)
                {
                    return compared;
                }
            }

            return 0;

            static Card MapToWeakJoker(
                Card c)
                => c is Card.J
                    ? Card.WeakJ
                    : c;
        }
    }

    private sealed record Hand(
        List<Card> Cards,
        int Bid)
    {
        public HandStrength StrengthSimple()
        {
            var grouped = Cards
                .GroupBy(static c => c)
                .Select(static g => (g.Key, Count: g.Count()))
                .OrderByDescending(static t => t.Count)
                .ThenByDescending(static t => t.Key)
                .ToList();

            return grouped switch
            {
                [(_, 5)] => HandStrength.FiveOfAKind,
                [(_, 4), (_, 1)] => HandStrength.FourOfAKind,
                [(_, 3), (_, 2)] => HandStrength.FullHouse,
                [(_, 3), (_, 1), (_, 1)] => HandStrength.ThreeOfAKind,
                [(_, 2), (_, 2), (_, 1)] => HandStrength.TwoPair,
                [(_, 2), (_, 1), (_, 1), (_, 1)] => HandStrength.OnePair,
                _ => HandStrength.HighCard
            };
        }

        public static Hand Parse(
            string line)
        {
            if (line.Split(" ") is [var hand, var bid])
            {
                return new Hand(
                    hand
                        .Select(ParseCard)
                        .ToList(),
                    int.Parse(bid));
            }

            throw new UnreachableException();
        }

        private static Card ParseCard(
            char c)
            => c switch
            {
                '2' => Card._2,
                '3' => Card._3,
                '4' => Card._4,
                '5' => Card._5,
                '6' => Card._6,
                '7' => Card._7,
                '8' => Card._8,
                '9' => Card._9,
                'T' => Card.T,
                'J' => Card.J,
                'Q' => Card.Q,
                'K' => Card.K,
                'A' => Card.A,
                _ => throw new ArgumentOutOfRangeException(nameof(c), c, null)
            };
    }

    private enum HandStrength
    {
        HighCard,
        OnePair,
        TwoPair,
        ThreeOfAKind,
        FullHouse,
        FourOfAKind,
        FiveOfAKind
    }

    private enum Card
    {
        WeakJ = 1,
        _2 = 2,
        _3,
        _4,
        _5,
        _6,
        _7,
        _8,
        _9,
        T,
        J,
        Q,
        K,
        A
    }

    public const string Ex =
        """
        32T3K 765
        T55J5 684
        KK677 28
        KTJJT 220
        QQQJA 483
        """;

    // https://adventofcode.com/2023/day/7/input
    public const string Input =
        """
        992QQ 265
        J5A7J 392
        99998 958
        335KQ 108
        J5352 916
        T55JT 966
        8876K 874
        9765A 195
        7T7TT 358
        2QT52 701
        KK66K 768
        8Q8A8 401
        87888 591
        J62J8 739
        QQQ4Q 525
        J6ATQ 173
        7QQ3Q 3
        393JJ 413
        Q77AJ 929
        K4442 962
        92Q9A 309
        QQQQ3 49
        85J63 363
        39399 597
        877KK 611
        6A882 632
        J97A4 737
        6JA65 2
        K2JK6 162
        29T42 9
        94J64 415
        93J33 786
        555JJ 927
        6QK74 920
        A999A 885
        97JJA 223
        57557 19
        QQ5T8 849
        6JK43 292
        3QQ33 616
        Q6A36 599
        55599 999
        8QQQ8 470
        5K533 886
        88998 587
        3J334 92
        TT6T6 143
        J88T8 883
        AJ986 283
        J4442 297
        5J476 252
        867A2 149
        J978A 405
        QA7QT 21
        555J2 911
        555TQ 637
        Q28QQ 132
        7KJ73 156
        QTQQT 588
        5K5Q6 618
        656T6 866
        3J232 196
        5A588 444
        3T8J3 178
        T4Q4Q 102
        TT666 860
        TAQ76 766
        Q4A55 702
        22228 447
        5JQ59 586
        6A669 779
        K6293 981
        23K22 961
        5TK55 798
        T5786 754
        7TT66 570
        A9572 38
        2Q2QQ 949
        8J9JJ 875
        Q878Q 567
        K553K 381
        QQAJK 130
        2T9JT 809
        QTA64 547
        56556 956
        38K8K 158
        84338 590
        J7762 52
        AQAAJ 712
        374AT 555
        Q666J 473
        A933J 621
        A8AJ8 176
        T9836 323
        54577 877
        7J7AA 979
        TJJ78 395
        88886 353
        84446 664
        J8J3Q 909
        Q35T6 78
        26942 35
        JJJJJ 628
        92772 716
        QAAQA 493
        94683 838
        4925J 756
        T8782 777
        JAAAA 467
        KK84T 218
        7J5KA 95
        7A8T2 708
        63T65 982
        QQ66Q 748
        5565K 615
        K9666 97
        699J9 662
        53553 458
        25252 800
        J52QA 551
        K56T5 308
        22J27 772
        KJK9K 116
        8K288 197
        ATT4T 487
        6KJ48 356
        2Q2Q8 910
        4579K 653
        7AAA7 253
        86J85 696
        K6A29 963
        8588T 850
        AA82J 18
        A6JAA 643
        7997J 289
        56557 489
        68JJ6 280
        6K7T3 895
        7J238 953
        54444 926
        K4466 484
        5Q34T 970
        935J7 648
        564Q3 477
        T2777 832
        8Q84Q 240
        4444T 8
        TQQQQ 636
        K8T8J 276
        7T845 755
        66J66 678
        A3478 329
        JQ9J6 386
        9J399 600
        797A9 802
        J6TT6 251
        54K5K 642
        KKK47 544
        J2333 845
        85895 113
        355J3 519
        54555 545
        77888 135
        88K55 673
        A5A58 585
        2222J 749
        54J52 234
        KQK3Q 824
        9K885 730
        KK2KK 144
        77TT7 245
        895Q7 221
        958J3 847
        T2TJ7 523
        82286 723
        T8T77 565
        2T2TT 229
        J46J7 339
        8JK83 580
        A8AAQ 596
        325JA 612
        K252K 890
        9K333 267
        66A6A 344
        55A52 506
        J5445 622
        2222T 357
        5QT67 762
        4A4AA 884
        67AAT 243
        6868Q 73
        6AA33 871
        QTQQ4 103
        33J33 844
        KJKKT 235
        AA3A3 819
        AT9K5 941
        7A777 522
        55954 419
        Q3JAJ 842
        QTTT8 576
        99299 796
        9K7A5 445
        5J556 921
        6QJAQ 706
        Q3QJ7 879
        TTTTQ 351
        63QQ7 594
        A4857 374
        3833K 801
        33855 990
        7J369 853
        59TQ2 313
        95K3T 633
        8K234 959
        QQJ7Q 951
        TJTAT 775
        639Q2 787
        A245K 331
        56555 159
        J8558 87
        8292Q 171
        8Q888 660
        5QA52 29
        4T96A 311
        586A7 603
        92289 757
        QTAT2 893
        95TJ2 396
        4K74K 294
        9JK66 154
        6QJT4 343
        Q58TK 122
        K5KKT 817
        65JTT 278
        T4Q3K 6
        J3444 336
        88889 699
        AKA99 693
        5A5J5 627
        4JJ44 488
        533TT 71
        66966 208
        JT6TA 823
        6K66K 148
        JK797 732
        5K426 426
        93739 443
        89696 354
        J8JJJ 215
        K4Q98 978
        4K8AK 367
        TT8T9 857
        3399Q 11
        TTTQ2 31
        5555Q 369
        97999 778
        545KQ 614
        76Q53 434
        4TQ74 137
        8JTTT 220
        85A59 711
        K3Q33 433
        Q3277 593
        555QJ 759
        A2772 492
        55395 75
        5555K 355
        KKKQQ 219
        22643 482
        T65T5 277
        93338 76
        55543 924
        7AA77 914
        KT94K 247
        7Q557 661
        QQQ6Q 936
        A5423 77
        222Q2 944
        64K28 435
        3222T 72
        QJJJJ 700
        4Q494 677
        TTTQJ 747
        QQ8AA 204
        478KT 299
        7638A 774
        K8KQ8 764
        67777 935
        522J2 498
        AJ5K8 360
        38488 370
        43AQ3 574
        26522 516
        39Q95 238
        T78KA 161
        6KKKK 410
        28228 399
        8QQQQ 799
        3875K 937
        J88J8 834
        333TT 539
        57533 505
        47748 12
        68J22 479
        4242J 318
        24AJ4 740
        67J58 589
        39845 290
        4A628 563
        59999 686
        JQ888 177
        AQ9QA 456
        99KA4 384
        466JK 813
        TJJ2K 805
        97687 319
        Q57Q5 656
        7747Q 609
        5T777 746
        Q77QQ 767
        8K483 361
        T8583 969
        KKJ88 56
        KT7T3 652
        73664 138
        975T6 815
        4T23Q 330
        Q4999 971
        T6Q6T 897
        28892 683
        QQQ63 341
        72T56 378
        22T29 239
        KKK3K 631
        7TTTT 327
        627J8 259
        6K924 534
        4TTT4 668
        JQQ9Q 828
        J995K 830
        34Q4K 598
        68J42 478
        446KK 791
        75J8T 437
        6A2J4 546
        AA277 33
        QQQQK 133
        43J86 758
        9J293 187
        TTATA 811
        33TAT 878
        66566 44
        T863A 372
        KT948 655
        TK77T 185
        87J79 974
        A333A 412
        4J665 382
        2J275 922
        AJTQK 500
        K97Q5 217
        K783T 713
        226Q2 455
        22TJT 242
        66677 64
        33533 814
        595J9 607
        29J9J 107
        657KK 905
        89998 938
        KJQAA 40
        55JK8 968
        3A84J 393
        5T447 862
        T2TQQ 303
        JQ949 783
        QJ432 671
        T5KQT 385
        QA697 818
        AA222 952
        74J7T 74
        2J8A2 997
        47457 923
        888K8 559
        T3J66 174
        324J9 23
        A8777 168
        A589K 841
        KT573 881
        2TJ28 441
        QAQAQ 421
        63233 669
        39333 50
        9A88J 566
        59347 595
        44A23 751
        85333 908
        88668 81
        K585K 967
        2A2AA 46
        88565 288
        AAKKK 364
        73A3A 605
        K4747 206
        66363 432
        KK87K 347
        K623T 904
        J33Q3 146
        Q9Q4J 851
        QK7KQ 298
        66K6A 972
        383J2 125
        T9Q4J 604
        4A444 635
        TT87T 424
        45JT5 495
        K628Q 928
        88A5A 314
        JK777 734
        76788 15
        8369Q 854
        5A55K 494
        55788 109
        2Q624 409
        773Q4 475
        J9396 129
        8T688 654
        A5KJA 512
        9Q899 254
        46T4K 250
        4T27J 785
        3TTT3 230
        JKK2K 991
        K5A43 651
        2T764 202
        JJ3KK 58
        86A68 993
        24AA3 666
        TT848 139
        AKA79 365
        KJ23J 422
        4T3K9 514
        88288 975
        Q7A5T 947
        978A8 48
        22TJ2 680
        AKK4T 429
        57444 846
        94994 507
        58AK3 427
        25525 430
        2223A 188
        26A2A 82
        49T9J 934
        AK6T8 55
        7T29A 795
        Q2A79 43
        854Q5 163
        333T3 201
        9792Q 246
        73653 985
        5QA56 562
        QQKKJ 550
        A9949 340
        T7TJT 623
        5Q7KT 126
        AKTAT 448
        9K299 903
        68J66 839
        86K6J 720
        33Q4Q 960
        44AQ8 368
        64666 62
        5J7A4 164
        44487 672
        67476 180
        44242 463
        4A3KT 491
        86K27 404
        AA557 536
        2TTTT 572
        A848J 94
        22626 117
        95K43 848
        22225 900
        33338 403
        QQ8QT 803
        7J3K6 745
        33663 281
        24558 124
        TQ297 554
        8T8KT 792
        T8TT6 771
        AA3AA 462
        9J99Q 438
        K5559 571
        Q95KT 794
        Q7777 337
        4J627 679
        AAA5A 994
        26Q9K 816
        3Q2Q2 509
        24992 84
        Q4QK3 123
        79798 843
        3K434 105
        T79AK 980
        7KK7J 915
        8998J 704
        37633 128
        KK7KK 51
        7A577 326
        666Q6 22
        999A9 172
        J99T9 695
        Q953J 532
        8A3KA 483
        A74Q4 725
        Q7QQQ 579
        KK775 153
        33535 248
        K79T4 925
        67575 349
        Q93J7 453
        8A395 390
        3K3K3 402
        62T62 317
        AAA69 25
        5A8K8 27
        KKKKA 513
        56656 568
        T5TT3 793
        8T8TT 346
        JA9A9 119
        99J99 305
        3A44K 258
        69999 531
        88488 988
        794J4 121
        955J4 634
        J99K8 529
        2Q33K 640
        6Q36Q 573
        46545 807
        9989T 394
        44542 216
        QQ88T 887
        J6898 90
        J8T7K 943
        KQQQ2 703
        AA9AA 869
        T2K37 54
        KKQ8A 865
        922J2 99
        7J796 776
        9J6T7 192
        5T9QJ 955
        72227 88
        K5455 440
        AT47Q 269
        8TK3A 388
        A8222 983
        Q87JA 485
        QQQJJ 682
        43334 157
        44853 780
        QQ6JQ 366
        54566 350
        K4928 237
        9QQQQ 852
        K9K96 342
        75827 907
        97QJ4 888
        288Q3 439
        584Q2 575
        69696 209
        55456 577
        JA999 541
        4T6T6 617
        6J454 377
        35JA6 262
        2KKK2 193
        6AJT9 733
        555TJ 549
        JTT2T 619
        T2JJ2 606
        82798 490
        885Q8 142
        92566 194
        93658 304
        94996 761
        QKKKA 375
        QKKKK 257
        9797Q 826
        63668 389
        7J6TJ 338
        87759 486
        26768 291
        5KAJ2 464
        J66J6 36
        2Q634 383
        KJKJK 724
        59T9T 957
        4Q34A 556
        27J72 373
        47KQT 992
        J93AK 867
        74296 940
        88QJ6 657
        22422 581
        333JJ 913
        4JTT8 537
        8977A 89
        T5K99 743
        333T7 718
        75A75 284
        J777J 279
        T8QA4 876
        44424 85
        ATA8T 41
        T22TA 896
        KKQ3T 380
        473K4 79
        JTT4J 508
        T2T72 328
        73577 474
        Q7744 984
        TA62Q 933
        2KQ2K 503
        QKJ6K 408
        964KT 630
        T56A6 67
        4499Q 91
        94A44 987
        944JJ 5
        Q4557 709
        63789 59
        2QJ32 714
        AJA4A 736
        AJ3AA 864
        33A34 325
        KTA4Q 821
        9592T 268
        JJ88K 231
        5Q5J8 760
        2QQQJ 710
        52A2J 452
        7Q287 899
        4292J 186
        425K8 222
        54584 451
        J2QQK 552
        55234 457
        K8K88 870
        J2AQK 729
        AT576 601
        A66JQ 564
        24JK6 53
        KKK4K 625
        QJ9K9 181
        KT6TK 411
        9QQ9Q 420
        6272K 727
        3JJT3 232
        TAA64 510
        32T23 773
        Q5A89 147
        9AJ9K 950
        67J5J 812
        4K737 502
        AQ458 726
        75J77 469
        3Q333 476
        69799 362
        89997 822
        39J73 295
        9T7TJ 273
        9JQ9A 348
        7657J 872
        92888 548
        QQ5QQ 533
        55J55 741
        55575 517
        AJ222 320
        5T75T 592
        55QTT 674
        98868 182
        3J6Q2 80
        J4Q65 322
        T8T88 528
        TKQJJ 542
        6T543 260
        7T3KJ 145
        Q9796 233
        A43A3 859
        8J982 315
        3A99Q 83
        2495T 207
        93QK6 882
        T83K3 249
        Q444Q 1000
        42658 717
        J3336 705
        J8JQ4 155
        62832 213
        26626 321
        37733 553
        TTT55 275
        Q7632 930
        J43K2 946
        7J877 538
        2595Q 894
        5QA68 111
        4J44Q 789
        55558 37
        945QT 244
        26546 301
        3QAQ4 697
        66464 7
        J6636 270
        7993A 676
        JK4JK 428
        47QJ4 917
        5KKJ5 471
        8787J 333
        4849Q 804
        3KK32 613
        2224J 670
        5A59T 101
        85585 658
        TJTTJ 310
        Q852Q 530
        K596A 629
        9A3Q7 753
        T9J47 203
        655T2 685
        29992 694
        82Q6Q 770
        83A38 735
        77337 459
        48344 744
        6888A 891
        KK44T 837
        97869 788
        TTJTK 497
        44744 264
        2KJ22 496
        J3K3K 335
        9KKKK 10
        AAAA2 57
        75T63 721
        4TQQ5 659
        7A8JT 398
        QT3QQ 93
        A2J69 191
        86342 825
        8499K 224
        8QK2T 540
        Q7QQ6 359
        T5T6K 535
        927T8 189
        8Q8JK 646
        5558J 645
        4448J 468
        JJ799 226
        77KK7 69
        T59KJ 520
        KAA9Q 227
        56QQ6 810
        33767 689
        JQQ33 274
        KAJQK 698
        KA8K8 263
        9QQ99 296
        J7A33 763
        7KT88 400
        TKTTT 602
        26TT2 684
        45734 781
        KKK5K 835
        65Q82 306
        3QQQ3 831
        8QK83 624
        Q35Q5 583
        Q85J6 480
        8T4TA 406
        Q56Q2 271
        44J45 345
        9888T 324
        36536 584
        8QAK5 352
        Q2QA7 561
        62269 675
        44449 106
        5TTK5 901
        JJQ4Q 167
        K96J2 391
        43J4Q 649
        6QQ6T 797
        J9799 855
        92J37 690
        46474 127
        A3333 964
        2AJAA 543
        AJAJ9 30
        45599 626
        9TQ44 26
        T9A47 692
        4285A 32
        999JJ 136
        8442A 989
        AAA9T 60
        Q6343 14
        TT2KK 880
        Q8232 316
        Q8742 302
        94646 214
        KQ46K 300
        J425J 511
        54488 965
        K5A62 65
        T55T5 715
        K8Q88 688
        8833T 225
        KQQQK 96
        484J8 836
        8K9JT 131
        75272 902
        4Q228 707
        A45J4 1
        QQ9Q8 504
        3JT73 742
        6Q636 557
        AQ572 667
        56667 293
        A3322 376
        8888J 650
        22329 973
        8TTTT 285
        AQ472 608
        497Q2 166
        JK852 371
        8AJAJ 856
        787TQ 66
        TJTTT 112
        46J67 47
        553A5 918
        39369 120
        75528 414
        QQQJQ 641
        A777K 118
        883QQ 518
        67K7J 449
        8A545 17
        K78AK 190
        J5546 282
        5TT92 639
        AAJ7A 4
        AKKK9 261
        KTKKK 976
        525T7 160
        7A239 307
        6T666 998
        462A6 266
        225J5 68
        77447 691
        K9QQ9 996
        6T62T 977
        9595Q 199
        322KK 898
        54QT2 665
        JAAAK 527
        2Q272 28
        AAAQA 769
        AAAA7 638
        T99T9 332
        JQ9QK 610
        T9T9T 931
        7KAK4 272
        92T56 808
        JQQ52 912
        JT2KQ 889
        6QQ4J 827
        37999 620
        3QJAA 286
        3999A 750
        8J6KK 387
        8A9A8 863
        4KK7J 175
        7QJAJ 731
        2J662 820
        4736J 45
        JAQ34 418
        77878 379
        273K3 86
        38892 465
        2K7J7 205
        45T54 236
        ATA2A 829
        Q2868 114
        5J7Q7 228
        7992K 134
        4TTKA 436
        99Q92 454
        K6JTT 892
        65J65 858
        QQ555 241
        767TQ 945
        T7744 954
        K6KJ6 840
        6656J 417
        2K44K 450
        86288 115
        AJAJA 140
        539T3 179
        J5K33 569
        47767 425
        333QA 752
        5TTT6 647
        7K7AK 165
        A76J5 70
        2Q7QK 256
        J68T8 334
        5JJ66 170
        3K3KA 446
        44JK4 861
        8JQ8J 472
        7878A 481
        4KQJ6 582
        4366A 919
        74Q52 20
        KK222 210
        QQKQA 287
        38Q98 521
        656T4 63
        44554 790
        8J2KA 200
        7TJ72 558
        JKKKK 98
        35687 442
        K286K 466
        4J586 784
        787Q8 906
        6J6K6 13
        44J44 738
        8J68J 431
        TQ935 526
        4434T 198
        7Q58K 183
        6J99A 61
        59QQQ 100
        KTTJK 184
        32743 719
        565AK 407
        7A839 42
        7JJ93 16
        749A6 765
        J65JT 423
        AQ229 806
        99JT3 397
        6A466 948
        9J4KK 151
        46JA6 110
        34939 939
        99559 169
        J75T7 255
        55542 681
        K84JK 722
        526T4 416
        95555 501
        AQJ49 995
        KQA69 833
        JT66J 104
        T443K 644
        7J777 986
        7AKAA 868
        4QT8K 211
        66KA9 663
        9962J 150
        267T2 34
        6444T 560
        86883 942
        QJT55 152
        6A5K4 687
        Q6T93 141
        22622 932
        65KK3 312
        AA6A6 461
        88876 499
        84KKK 212
        5A555 782
        5T655 39
        552K2 515
        Q3QQ5 24
        393KK 524
        22K86 873
        72KT4 728
        22JJ2 578
        3K733 460
        """;
}
