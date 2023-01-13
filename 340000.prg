1 ' ===================================
2 '
3 '  2100301001 STEP5 Assy1プログラム
4 '
5 ' 作成者：M.Hayakawa
6 ' 作成日：2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1から流用
8 ' ===================================
9 '===== <Insight定数> =====
10 '===== <Insight変数定義> =====
11 Dim PInspPosition(30)               '画像処理Function引渡し用位置変数
12 Dim MInspGroup%(30)                 '画像処理Function引渡し用変数
13 Def Inte MIN_IS_Ready               '【入力IO】Insight準備OK
14 Def Inte MIN_IS_JobLoadOK           '【入力IO】Insightジョブロード正常終了
15 Def Inte MIN_IS_JobLoadNG           '【入力IO】Insightジョブロード異常終了
16 Def Inte MIN_IS_InspGSetOK          '【入力IO】Insight検査グループ番号設定正常終了
17 Def Inte MIN_IS_InspGSetNG          '【入力IO】Insight検査グループ番号設定異常終了
18 Def Inte MIN_IS_InspOK              '【入力IO】Insight検査OK
19 Def Inte MIN_IS_InspNG              '【入力IO】Insight検査NG
20 Def Inte MIN_IS_InspErr             '【入力IO】Insight検査異常終了
21 Def Inte MIN_IS_InspCapDone         '【入力IO】Insight検査画像取込完了
22 '
23 Def Inte MIN_IS_ErrNum              '【入力IO】Insight処理エラー番号取得開始アドレス(16bit)
24 'Output Signal
25 Def Inte MOUT_IS_JobLoadReq         '【出力IO】Insight JOBロード要求
26 Def Inte MOUT_IS_InspGSetReq        '【出力IO】Insight 検査グループ番号設定要求
27 Def Inte MOUT_IS_Insp               '【出力IO】Insight 検査実行要求
28 '
29 Def Inte MOUT_IS_JobNum             '【出力IO】Insight JOB番号設定開始アドレス(16bit)
30 Def Inte MOUT_IS_InspGNum           '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
31 '
32 Def Inte MOUT_InspErrNum            '【出力IO】検査実行エラー番号開始アドレス(16bit)
33 Def Inte MOUT_InspNGStepNum         '【出力IO】検査実行NGStep番号開始アドレス(16bit)
34 '作業用変数
35 Def Inte MInspErrNum                '検査実行エラー番号
36 Def Inte MInspNGStepNum             '検査実行NGStep番号
37 Def Inte MRtn                       'Function戻り値取得用
38 Def Inte MRtn2                      'Function戻り値取得用
39 Def Inte MRet3                      'Function戻り値取得用
40 Def Inte MGRtn                      'Function戻り値取得用 ネジ供給機
41 Def Inte MInspErrNumSub             '検査実行エラー番号sub　20190820追加
42 Def Inte MovrdA                     'ネジ締めOvrd 可変用
43 Def Float MSpdA                     'ネジ締めSpd　可変用
44 Def Pos PTemp                       'ネジ締め上空位置計算用
45 '===== <Insight変数設定> =====
46 MIN_IS_Ready%        =   11380      '【入力IO】Insight準備OK
47 MIN_IS_JobLoadOK%    =   11381      '【入力IO】Insightジョブロード正常終了
48 MIN_IS_JobLoadNG%    =   11382      '【入力IO】Insightジョブロード異常終了
49 MIN_IS_InspGSetOK%   =   11383      '【入力IO】Insight検査グループ番号設定正常終了
50 MIN_IS_InspGSetNG%   =   11384      '【入力IO】Insight検査グループ番号設定異常終了
51 MIN_IS_InspOK%       =   11385      '【入力IO】Insight検査OK
52 MIN_IS_InspNG%       =   11386      '【入力IO】Insight検査NG
53 MIN_IS_InspErr%      =   11387      '【入力IO】Insight検査異常終了
54 MIN_IS_InspCapDone%  =   11388      '【入力IO】Insight検査画像取込完了
55 MIN_IS_ErrNum%       =   11408      '【入力IO】Insight処理エラー番号開始アドレス(16bit)
56 'Output Signal
57 MOUT_IS_JobLoadReq%  =   12370      '【出力IO】Insight JOBロード要求
58 MOUT_IS_InspGSetReq% =   12371      '【出力IO】Insight 検査グループ番号設定要求
59 MOUT_IS_Insp%        =   12372      '【出力IO】Insight 検査実行要求
60 MOUT_IS_JobNum%      =   12384      '【出力IO】Insight JOB番号設定開始アドレス(16bit)
61 MOUT_IS_InspGNum%    =   12400      '【出力IO】Insight 検査グループ番号設定開始アドレス(16bit)
62 MOUT_InspErrNum%     =   12416      '【出力IO】検査実行エラー番号開始アドレス(16bit)
63 MOUT_InspNGStepNum%  =   12432      '【出力IO】検査実行NGStep番号開始アドレス(16bit)
64 '===== <電ドラ変数定義> =====
65 X20_Driver=11248                    '電ドラステイタス1　Driver Status 1
66 X21_Driver=11249 '電ドラステイタス2  Driver Status 2
67 X22_Driver=11250 '電ドラステイタス3  Driver Status 3
68 X23_Driver=11251 '電ドラステイタス4  Driver Status 4
69 X24_Driver=11252 '電ドラエラーメッセージ1 Driver Error E1
70 X25_Driver=11253 '電ドラエラーメッセージ2 Driver Error E2
71 X26_Driver=11254 '電ドラエラーメッセージ3 Driver Error E3
72 X27_Driver=11255 '電ドラエラーメッセージ4 Driver Error E4
73 X28_Driver=11256 '電ドラトータルエラーシグナル Total Error
74 X29_Driver=11257 '電ドラ終了シグナル Comlete signal
75 X2A_Driver=11258 '電ドラエラーメッセージ5 Driver Error E5
76 '11584   'toRBトルクドライバ-COMP_ERR送信
77 Y60_Driver=12240 '電ドラ半時計回り CCW
78 Y61_Driver=12241 '電ドラ時計回り CW
79 Y62_Driver=12242 'バンクセッティング BANK C1
80 Y63_Driver=12243 'バンクセッティング BANK C2
81 Y64_Driver=12244 'バンクセッティング BANK C3
82 Y65_Driver=12245 'プログラムセッティング PRG SET F1
83 Y66_Driver=12246 'プログラムセッティング PRG SET F2
84 Y67_Driver=12247 'プログラムセッティング PRG SET F3
85 X34_ScrewReady1=11259 'ねじっこ1　Read
86 '===== <電ドラ定数> =====
87 Dim PScrewPos(10)       'ネジ締め用Function引数変数
88 Dim PGetScrewPos(10)    'ねじ供給機からねじを得るFunction引数変数
89 Dim PEscapePosi(10)
90 MLoopCnt% = 0'
91 '===== <ロボット定数> =====
92 '===== <ロボット変数定義> =====
93 MRBTOpeGroupNo = 0      'ロボット動作番号初期化
94 MCommentD1001 = 0
95 MCommentD1002 = 0
96 MCommentD1003 = 0
97 MScreenNo = 0
98 '
99 MCommentTSU = 0
100 MCommentTSD = 0
101 'ウィンド画面番号設定
102 MWindReSet = 0
103 MWindInfoScr = 5
104 MWindErrScr = 10
105 MWindErrScr2 = 11
106 MWindErrScr3 = 13
107 MWindErrScr17 = 17
108 MWindErrScr18 = 18
109 MWindCmmnScr = 20
110 MWindJigRelase19049 = 60
111 MWindJigRelase19050 = 61
112 MWindJigRelase19051 = 62
113 '
114 MClear% = 0        'KEY_のクリア
115 MAbout% = 1        'KEY_停止
116 MNext% = 2         'KEY_次のステップへ移行
117 MContinue% = 3     'KEY_継続 再度同じ動作を行う
118 '
119 Def Inte MNgProcess
120 MNgProcess% = 5      'KEY_NG
121 '
122 MAssyOK% = 6       '組立完了
123 MPass% = 7         '工程パス
124 MPiasNG% = 8       'Pias確認時履歴NG
125 '
126 '初期化用KEY番号   '
127 MRobotInit1% = 11  '初期位置用
128 MRobotInit2% = 12  '初期位置用
129 MRobotInit3% = 13  '初期位置用
130 MRobotInit4% = 14  '初期位置用
131 '
132 MIN_INIT1REQUEST% = 11568 'toRBT_ロボット初期位置1要求
133 MIN_INIT2REQUEST% = 11569 'toRBT_ロボット初期位置2要求
134 MIN_INIT3REQUEST% = 11570 'toRBT_ロボット初期位置3要求
135 MIN_INIT4REQUEST% = 11571 'toRBT_ロボット初期位置4要求
136 '
137 MOUT_INIT1RECIVE% = 12560 'toPLC_ロボット初期位置1受信
138 MOUT_INIT2RECIVE% = 12561 'toPLC_ロボット初期位置2受信
139 MOUT_INIT3RECIVE% = 12562 'toPLC_ロボット初期位置3受信
140 MOUT_INIT4RECIVE% = 12563 'toPLC_ロボット初期位置4受信
141 '
142 MOK% = 1               '各判定用
143 MNG% = 0               '各判定用
144 MTIMEOUT% = -1         '各判定用
145 MJudge% = 0            '判定情報格納用
146 '
147 MRECIVETIME& = 0
148 MSETTIMEOUT10& = 10000&                '10秒設定
149 MSETTIMEOUT03& = 3000&                 '3秒設定
150 MSETTIMEOUT01& = 1000&                 '1秒設定
151 MSETTIMEOUT05& = 5000&                 '5秒設定
152 MSETTIMEOUT009& = 900&                 '0.9秒設定
153 MSETTIMEOUT008& = 800&                 '0.8秒設定
154 MSETTIMEOUT007& = 700&                 '0.7秒設定
155 MSETTIMEOUT006& = 600&                 '0.6秒設定
156 MSETTIMEOUT005& = 500&                 '0.5秒設定
157 MSETTIMEOUT004& = 400&                 '0.4秒設定
158 MSETTIMEOUT003& = 300&                 '0.3秒設定
159 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
160 MIN_PIAS_ComOK% = 11552                'PC通信OK
161 MIN_PIAS_ComTimeOut% = 11576           'PC通信確認タイムアウト
162 MIN_PIAS_ComNG% = 11553                'PC通信NG
163 MOUT_PIAS_ComCheck% = 12544            'PC通信確認要求
164 MOUT_PIAS_Missing_Process% = 12546     '工程抜け確認要求
165 MIN_PIAS_ModelTypeNG% = 11554          'モデル仕向NG
166 MIN_PIAS_ProcessHistryNG% = 11555      '前工程履歴NG
167 MIN_PIAS_ProcessHistryOK% = 11556      '前工程履歴OK
168 MIN_PIAS_ProcessHistryErr% = 11557     '工程履歴処理エラー
169 MIN_PIAS_MyProcessComp% = 11573        '自工程履歴あり
170 MIN_PIAS_ProcessHistryTimeOut% = 11578 '工程履歴タイムアウト
171 MOUT_OKNG% = 12549                     'PLC OUT でOK=1, NG=0 出力
172 '
173 MOUT_PiasPCBNumberCheck = 12557        '基板番号照合
174 MIN_PiasPCBNumberOK% = 11566          '基板番号OK
175 MIN_PiasPCBNumberNG% = 11565          '基板番号NG
176 MIN_PiasPCBNumberErr% = 11567         '基板番号処理エラー
177 '
178 MOUT_PiasAssyResultOK% = 12549    '組立OK
179 MOUT_PiasAssyResultNG% = 12550    '組立NG
180 MOUT_PiasAssyResultWr% = 12548    '工程履歴書き込み
181 '
182 MIN_PiasProcessNG% = 11559        '工程履歴処理NG
183 MIN_PiasProcessOtherErr% = 11560  '工程履歴処理エラー(なんかのトラブル)
184 MIN_PiasProcessOK% = 11558        '工程履歴処理OK
185 '
186 MIN_Insight_Use% = 11369               '画像確認ON
187 MIN_TorqueCheck% = 11348               'トルクチェック
188 '
189 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT操作権
190 MOUT_RED_LIGHT% = 12356            'PATLIGHT 赤 点灯
191 MOUT_RED_FLASH% = 12357            'PATLIGHT 赤 点滅
192 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT 黄 点灯
193 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT 黄 点滅
194 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT 青 点灯
195 MOUT_GREEN_FLASH% = 12361          'PATLIGHT 青 点滅
196 '
197 MOUT_ST_DATETIME% = 12551          '組立開始日付時刻
198 MOUT_ED_DATETIME% = 12552          '組立終了日付時刻
199 '
200 MOUT_TORQUE_CHECK% = 12367         'PLCへトルクチェック中を送信
201 '
202 MIN_ASSY_CANCEL% = 11366           '組立を行うかのフラグ
203 '
204 MLoopFlg% = 0                      'KEY入力後のOK or NG内容
205 MopeNo% = 0
206 MRtn% = 0
207 MRet = 0
208 MRet3% = 0
209 '
210 Def Inte MInputQty          '投入数 演算変数
211 Def Inte MAssyOkQty         '組立ＯＫ数 演算変数
212 Def Inte MAssyNgQty         '組立ＮＧ数 演算変数(未使用)
213 Def Inte MSuctionErrQty     '吸着エラー数 演算変数 2022/04/27 渡辺
214 Def Inte nAssyOkQty         '未使用
215 Def Inte MScrewNo
216 Def Inte MReTry
217 '===== <IO変数定義> =====
218 Def Inte MIN_VS1            ' アーム先端　ネジ吸着センサ1
219 'Def Inte MIN_VS2           ' アーム先端　ネジ吸着センサ2　→　アイオー点数足りないため廃止
220 Def Inte MIN_CS13           ' アーム先端　シャシ・サポートCy戻端　検出
221 Def Inte MIN_CS1            ' アーム先端　MainPWB用チャック閉検出
222 Def Inte MIN_CS2            ' アーム先端　MainPWB用チャック開検出
223 Def Inte MIN_CS3            ' アーム先端　サブシャシ用チャック閉検出
224 Def Inte MIN_CS4            ' アーム先端　サブシャシ用チャック開検出
225 Def Inte MIN_PSE1           ' アーム先端　ワーク検出光電SW
226 '
227 Def Inte Y68_VV1            ' アーム先端　ネジ吸着バルブ
228 Def Inte Y6B_VB1            'アーム先端　吸着破壊バルブ
229 Def Inte MOUT_VB1           ' アーム先端　ネジ吸着破壊バルブ
230 '
231 Def Inte MIN_CS5            ' ベース側　SubChassisプッシャCy戻端　検出
232 Def Inte MIN_CS6            ' ベース側　SubChassisプッシャCy出端　検出
233 Def Inte MIN_CS7            ' ベース側　スライドL･Cy戻端 検出
234 Def Inte MIN_CS8            ' ベース側　スライドL･Cy出端 検出
235 Def Inte MIN_CS9            ' ベース側　スライドR･Cy戻端 検出
236 Def Inte MIN_CS10           ' ベース側　スライドR･Cy出端 検出
237 Def Inte MIN_CS11           ' ベース側　クランプCy戻端 検出
238 Def Inte MIN_CS12           ' ベース側　クランプCy出端 検出
239 Def Inte MIN_PSE2           ' ベース側　機種判別センサ1
240 Def Inte MIN_PSE3           ' ベース側　機種判別センサ2
241 '
242 Def Inte MOUT_SV9           ' ベース側　プッシャCy用SV(onで位置決め方向)
243 Def Inte MOUT_SV10          ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
244 Def Inte MOUT_SV11          ' ベース側　MainPWB持ち上げ防止Cy用SV
245 '
246 Def Inte MOUT_LED1          ' 画像処理用LED照明
247 '
248 Def Inte MNEJI_COUNTS       ' ねじ締める本数カウントアップ用変数
249 Def Inte MNEJI_G_ERR_COUNTS ' ねじ供給連続エラーカウントアップ用変数
250 '
251 Def Inte MSTORE_INP_ADD     '　入力時間監視対象のアドレスを入力
252 Def Inte MCOUNT_UP_SEC      '　センサ入力WaitTimerのカウンター　msec
253 Def Inte MCOUNT_UP_LIM      '　センサ入力WaitTimerのカウントアップ時間　msec
254 Def Inte MCOUNT_UP_JUDG     '　センサ入力WaitTimerの戻り判定値　0→NG　1→OK　2→カウントアップ中
255 Def Inte MCHUCK_RET_COUNTS  '  チャッキング・連続リトライ・カウントアップ用変数
256 Def Inte MCLUMP_RET_COUNTS  '  サブシャシ・クランプ・連続リトライカウントアップ用変数
257 '
258 Def Inte MOUT_Y7E_BACKUP    '  サブシャーシ変形対策治具 2020-02-06
259 Def Inte MIN_X32_BACKUP_IN  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
260 Def Inte MIN_X33_BACKUP_OUT '  サブシャーシ変形対策治具 出センサー2020-02-06
261 '
262 MIN_VS1%    =  11259    ' アーム先端　ネジ吸着センサ1
263 MIN_CS13%   =  11260    ' アーム先端　シャシ・サポートCy戻端　検出
264 MIN_CS1%    =  11261    ' アーム先端　MainPWB用チャック閉検出
265 MIN_CS2%    =  11262    ' アーム先端　MainPWB用チャック開検出
266 MIN_CS3%    =  11263    ' アーム先端　サブシャシ用チャック閉検出
267 MIN_CS4%    =  11264    ' アーム先端　サブシャシ用チャック開検出
268 MIN_PSE1%   =  11265    ' アーム先端　ワーク検出光電SW
269 Y68_VV1%    =  12248    ' アーム先端　ネジ吸着バルブ '数値12250から12248へ変更(8/5中村)
270 Y6B_VB1%    =  12250    'アーム先端　吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
271 MOUT_VB1%   =  12250    ' アーム先端　ネジ吸着破壊バルブ  '数値12251から12250へ変更(8/5中村)
272 '
273 MIN_CS5%    =  11269    ' ベース側　SubChassisプッシャCy戻端　検出
274 MIN_CS6%    =  11270    ' ベース側　SubChassisプッシャCy出端　検出
275 MIN_CS7%    =  11271    ' ベース側　スライドL･Cy戻端 検出
276 MIN_CS8%    =  11272    ' ベース側　スライドL･Cy出端 検出
277 MIN_CS9%    =  11273    ' ベース側　スライドR･Cy戻端 検出
278 MIN_CS10%   =  11274    ' ベース側　スライドR･Cy出端 検出
279 MIN_CS11%   =  11275    ' ベース側　クランプCy戻端 検出
280 MIN_CS12%   =  11276    ' ベース側　クランプCy出端 検出
281 MIN_PSE2%   =  11277    ' ベース側　機種判別センサ1
282 MIN_PSE3%   =  11278    ' ベース側　機種判別センサ2
283 '
284 MOUT_SV9%   =  12267    ' ベース側　プッシャCy用SV(onで位置決め方向)
285 MOUT_SV10%  =  12268    ' ベース側　スライドLR･Cy用SV(onで位置決め方向)
286 MOUT_SV11%  =  12269    ' ベース側　MainPWB持ち上げ防止Cy用SV
287 '
288 MOUT_LED1%  =  12239    ' 画像処理用LED照明
289 '
290 MOUT_Y7E_BACKUP% = 12270    '  サブシャーシ変形対策治具 2020-02-06
291 MIN_X32_BACKUP_IN% = 11267  '  サブシャーシ変形対策治具 戻りセンサー2020-02-06
292 MIN_X33_BACKUP_OUT% = 11266 '  サブシャーシ変形対策治具 出センサー2020-02-06
293 '
294 '共通
295 Def Inte MTEST_KEY                      'デバックテスト用
296 Def Inte MOn                            '出力=1
297 Def Inte MOff                           '出力=0
298 '
299 'ねじ締め装置_出力アドレス
300 Def Inte MOUT_ScwT_ComChk               '通信確認
301 Def Inte MOUT_ScwT_ST                   'ねじ締め開始
302 Def Inte MOUT_ScwT_FinOK                'ねじ締め完了受信を送信
303 Def Inte MOUT_ScwT_Case1OK              '条件1停止受信を送信
304 Def Inte MOUT_ScwT_Case2OK              '条件2停止受信を送信
305 Def Inte MOUT_ScwT_Case3OK              '条件3停止受信を送信
306 Def Inte MOUT_ScwT_Case4OK              '条件4停止受信を送信
307 Def Inte MOUT_ScwT_Case5OK              '条件5停止受信を送信
308 'ねじ締め装置_入力アドレス
309 Def Inte MIN_ScwT_comOK                 '通信確認返信
310 Def Inte MIN_ScwT_STRec                 'ねじ締め開始を受信
311 Def Inte MIN_ScwT_Fin                   'ねじ締め完了を受信
312 Def Inte MIN_ScwT_Case1                 '条件1停止を受信
313 Def Inte MIN_ScwT_Case2                 '条件2停止を受信
314 Def Inte MIN_ScwT_Case3                 '条件3停止を受信
315 Def Inte MIN_ScwT_Case4                 '条件4停止を受信
316 Def Inte MIN_ScwT_Case5                 '条件5停止を受信
317 '
318 Dim MScwT_Case1%(2)               '条件1停止変数
319 Dim MScwT_Case2%(2)               '条件2停止変数
320 Dim MScwT_Case3%(2)               '条件3停止変数
321 Dim MScwT_Case4%(2)               '条件4停止変数
322 Dim MScwT_Case5%(2)               '条件5停止変数
323 '
324 '共通
325 MTEST_KEY% = 11359                       'デバッグ用テストKEY
326 MOn% = 1                                 '出力 = 1
327 MOff% = 0                                '出力 = 0
328 '
329 'ねじ締め機_アドレス設定
330 MOUT_ScwT_ComChk% = 12816               '通信確認送信
331 MOUT_ScwT_ST% = 12849                   'ねじ締め開始を送信
332 MOUT_ScwT_ReSTOK% = 12850               '再開始受信を送信
333 MOUT_ScwT_FinOK% = 12852                'ねじ締め完了受信を送信
334 MOUT_ScwT_Case1OK% = 12858              '条件1停止受信を送信
335 MOUT_ScwT_Case2OK% = 12859              '条件2停止受信を送信
336 MOUT_ScwT_Case3OK% = 12860              '条件3停止受信を送信
337 MOUT_ScwT_Case4OK% = 12861              '条件4停止受信を送信
338 MOUT_ScwT_Case5OK% = 12862              '条件5停止受信を送信
339 '
340 MIN_ScwT_comOK% = 11824                 'ねじ締め装置から返信
341 MIN_ScwT_STRec% = 11857                 'ねじ締め開始を受信
342 MIN_ScwT_ReST% = 11858                  '再開始を受信
343 MIN_ScwT_Fin% = 11860                   'ねじ締め完了を受信
344 MIN_ScwT_Case1% = 11866                 '条件1停止待機を受信
345 MIN_ScwT_Case2% = 11867                 '条件2停止待機を受信
346 MIN_ScwT_Case3% = 11868                 '条件3停止待機を受信
347 MIN_ScwT_Case4% = 11869                 '条件4停止待機を受信
348 MIN_ScwT_Case5% = 11870                 '条件5停止待機を受信
349 '
350 MScwT_Case1%(1) = MIN_ScwT_Case1%
351 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
352 MScwT_Case2%(1) = MIN_ScwT_Case2%
353 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
354 MScwT_Case3%(1) = MIN_ScwT_Case3%
355 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
356 MScwT_Case4%(1) = MIN_ScwT_Case4%
357 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
358 MScwT_Case5%(1) = MIN_ScwT_Case5%
359 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
360 '
361 '設定 InitialZoneBで使用する変数
362 Def Pos PActive       '直交座標系 位置変数 現在位置
363 Def Pos Pmove         '直交座標系 位置変数 移動先
364 Def Jnt JActive       '関節座標系 位置変数 現在位置
365 Def Jnt Jmove         '関節座標系 位置変数 移動先
366 Def Jnt JTaihi        '関節座標系 位置変数 退避ポジション ティーチングで設定
367 Def Inte MRecoveryPass      '復帰動作パスフラグ　1=復帰動作をパス　0=復帰動作を実行
368 Def Inte MStandby              '待機位置確認フラグ
369 Def Inte MRecoveryChuckOpen    'チャック解放フラグ（復帰動作前）両掴み対策
370 '★注意★初期位置を変更した時には、変更が必要！
371 '
372 '===== 【位置変数(要・ティーチング） 説明、定義】 =====
373 Function M% fnAssyStart
374     M_25# = 0
375     M_26# = 0
376 ' PIASチケット読込み工程抜け確認
377     M_20# = MClear%                       '初期化
378 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
379 '        MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
380 '        '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
381 '        '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
382 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' メニューへ戻る
383 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NGを工程履歴に書込み次の工程へ
384 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NGを工程履歴に書込み次の工程へ
385 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' 履歴NG, 工程抜け
386 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK以外は組立終了
387 '    EndIf
388     ' ネジ締め機テスト用 ----------
389     '    'Mret% = fScewTcomChk()
390     '    'ねじ締め開始
391     '    fScewTStart()
392     '    '
393     '    '座標移動
394     '    '
395     '    '条件xx停止
396     '    fScewTCaseStop(MScwT_Case5%)
397     '    '
398     '    'ベースユニットKEY
399     '    Wait M_In(MTEST_KEY%) = MOn%
400     '    '
401     '    '再開始
402     '    fScewTReStart()
403     '    '
404     '    '座標移動
405     '    '
406     '    'ねじ締め完了
407     '    Mret% = fScewTFinish()
408     ' ネジ締めテスト終了
409     ' PIASテスト -----------
410     '    MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
411     '    MRet% = fnPiasWrite(MNG%)
412     '    MRet% = fnPCBNumberCheck()
413     ' PIASテスト終了 -------
414     '組み立て開始
415     'プログラム原点
416         '初期位置を設定
417     PTemp = P_Curr
418     MRtn = 0
419 '    If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
420 '        If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
421 '            If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
422 '                MRtn = 1
423 '            EndIf
424 '        EndIf
425 '    EndIf
426 '    If MRtn = 1 Then
427 '        M_Out(12269) = 0            '位置決め戻OFF
428 '        M_Out(12268) = 1            '位置決め出ON
429 '        Mov PTicketRead
430 '    Else
431 '        Mov PInitialPosition
432 '        M_Out(12269) = 0            '位置決め戻OFF
433 '        M_Out(12268) = 1            '位置決め出ON
434 '        Mov PTicketRead_1           'チケットID読み取り回避点
435 '        Mvs PTicketRead             'ID読み位置
436 '    EndIf
437 '
438 ' 2022/04/12 安全方向へ条件変更 渡辺
439 ' PInitialPosition 在席 MStandby=2
440 ' PTicketRead_1 在席 MStandby=1
441 '
442     MStandby = 0    '待機位置フラグを初期化
443     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
444         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
445             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
446                 MStandby = 2
447             EndIf
448         EndIf
449     EndIf
450     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
451         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
452             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
453                 MStandby = 1
454             EndIf
455         EndIf
456     EndIf
457     fnAutoScreenComment(521)        '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
458     If MStandby = 2 Then
459         M_Out(12269) = 0            '位置決め戻OFF
460         M_Out(12268) = 1            '位置決め出ON
461         Mov PTicketRead_1           'チケットID読み取り回避点
462         Mvs PTicketRead             'ID読み位置
463     EndIf
464     If MStandby = 1 Then
465         M_Out(12269) = 0            '位置決め戻OFF
466         M_Out(12268) = 1            '位置決め出ON
467         Mvs PTicketRead             'ID読み位置
468     EndIf
469     If MStandby <> 0 Then GoTo *PositionOK
470     fErrorProcess(11,230,281,0)           '初期位置にいない時はエラーにする
471     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
472     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
473     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
474     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
475     *PositionOK
476 '
477     Ovrd 100
478     'ハンド及び治具に本体が無いか
479     *INITIAL_CHECK
480     If M_In(11264) =0 And M_In(11269) = 0 Then GoTo *CompInitial
481     fErrorProcess(11,253,281,0)
482     If M_20# = MNext% Then M_20# = MClear%
483     If M_20# = MNgProcess% Then M_20# = MAbout%
484     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
485     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
486     *CompInitial
487 '
488     '治具を初期位置に戻す(追加11/19中村)
489     *RE_JIG_INI
490     MRtn = 1
491     MRtn2 = 1
492     If M_In(11276) = 0 Or M_In(11277) = 0 Then  '回転軸がセンターに来ていなければ
493         M_Out(12258) = 0        '製品チャック開OFF
494         M_Out(12259) = 1        '製品チャック閉ON
495         MRtn = frInCheck(11271,1,MSETTIMEOUT05&)    '製品チャック閉検出
496         M_Out(12262) = 0        '回転ストッパー出OFF
497         M_Out(12263) = 1        '回転ストッパー戻ON
498         MRtn2 = frInCheck(11275,1,MSETTIMEOUT05&)    '回転ストッパー戻端検出
499     EndIf
500     If MRtn = 1 And MRtn2 = 1 Then GoTo *CompJigIni1
501     fErrorProcess(11,262,284,0) '0→262に変更6/7中村
502     If M_20# = MNext% Then M_20# = MClear%
503     If M_20# = MNgProcess% Then M_20# = MAbout%
504     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
505     If M_20# = MContinue% Then GoTo *RE_JIG_INI
506 *CompJigIni1
507     '
508     If M_In(11278) = 1 Then     'CW端に回転していたなら
509         M_Out(12266) = 0        'バルブ2ONをOFF
510         M_Out(12267) = 1 Dly 0.3       'バルブ2OFFをON
511         Dly 0.3
512         M_Out(12264) = 0        'バルブ1ONをOFF
513         M_Out(12265) = 1 Dly 0.3       'バルブ1OFFをON
514     ElseIf M_In(11279) = 1 Then 'CCW端に回転していたなら
515         M_Out(12264) = 0        'バルブ1ONをOFF
516         M_Out(12265) = 1 Dly 0.3       'バルブ1OFFをON
517         Dly 0.3
518         M_Out(12266) = 0        'バルブ2ONをOFF
519         M_Out(12267) = 1 Dly 0.3       'バルブ2OFFをON
520     Else
521         M_Out(12264) = 0        'バルブ1ONをOFF
522         M_Out(12265) = 1 Dly 0.3       'バルブ1OFFをON
523         M_Out(12266) = 0        'バルブ2ONをOFF
524         M_Out(12267) = 1 Dly 0.3       'バルブ2OFFをON
525     EndIf
526     '
527 '    Wait M_In(11276) = 1 Or M_In(11277) = 1     '回転センター検出
528     MRtn = frInCheck(11276,1,MSETTIMEOUT05&)    '回転センター検出
529     MRtn2 = frInCheck(11277,1,MSETTIMEOUT05&)
530 '
531     If MRtn = 1 Or MRtn2 = 1 Then GoTo *CompJigIni2
532     fErrorProcess(11,265,284,0)
533     If M_20# = MNext% Then M_20# = MClear%
534     If M_20# = MNgProcess% Then M_20# = MAbout%
535     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
536     If M_20# = MContinue% Then GoTo *RE_JIG_INI
537 *CompJigIni2
538     '
539     '
540     If M_In(11271) = 1 Then     '製品チャック閉ならば
541         M_Out(12259) = 0        '製品チャック閉OFF
542         M_Out(12258) = 1        '製品チャック開ON
543 '        Wait M_In(11271) = 1    '製品チャック開検出
544         M_Out(12263) = 0        '回転ストッパー戻OFF
545         M_Out(12262) = 1        '回転ストッパー出ON
546 '        Wait M_In(11274) = 1    '回転ストッパー出端検出
547     EndIf
548     '
549     M_Out(12261) = 0            '製品クランパー引OFF
550     M_Out(12260) = 1            '製品クランパー出ON
551 '    Wait M_In(11272) = 1        '製品クランパー出端検出
552     '
553     M_Out(12256) = 0            'チャック閉OFF
554     M_Out(12257) = 1            'チャック開ON
555 '    Wait M_In(11265)            'チャック開センサーON
556 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'チャック開センサーON
557 '    If MRtn = 0 Then
558 '        fErrorProcess()         'エラー処理
559 '    EndIf
560     '
561 ''    Mov PInitialPosition
562 MRtn = 1        'MRtn初期化
563 'チケットIDを読む
564 *RE_TICKET_READ
565 If M_20# = MContinue% Then M_20# = MClear%
566 'PInspPosition(1) = PTicketRead  'IDチケット読取位置
567 'MInspGroup%(1) = 1              '検査G番号
568 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
569 M_20# = MClear%                       '初期化
570 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON時のみ実行
571     MRtn = fnPiasCheck()            'PIASチケットを読込み、確認
572     '通信確認外部変数操作（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
573     '工程抜け確認外部変数操作（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
574 EndIf
575 If MRtn = 1 Then GoTo *CompRead
576 'fErrorProcess(11,214,251,0)
577 'If M_20# = MNext% Then M_20# = MClear%
578 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
579 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
580 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
581 If M_20# = MNext% Then M_20# = MPass%
582 Mvs PTicketRead_1                         '22/04/12 追加 渡辺
583 GoTo *ASSY_ERROR_END
584 *CompRead
585     fnAutoScreenComment(521)        '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
586     '
587     'パレットから製品を取る
588     M_Out(12269) = 0            '位置決め戻OFF
589     M_Out(12268) = 1            '位置決め出ON
590     Mov PProductOnPltGet_2      '製品上空回避点
591     '
592     *RE_PLT_GET
593     '
594     M_Out(12269) = 0            '位置決め戻OFF
595     M_Out(12268) = 1            '位置決め出ON
596     M_Out(12256) = 0            'チャック閉OFF
597     M_Out(12257) = 1            'チャック開ON
598 '
599 '    Wait M_In(11265)            'チャック開センサーON
600     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'チャック開センサーON
601     If MRtn = 1 Then GoTo *CompPltGet1
602     fErrorProcess(11,244,284,0)
603     If M_20# = MNext% Then M_20# = MClear%
604     If M_20# = MAbout% Or M_20# = MNgProcess% Then
605         Mov PInitialPosition    '退避ルート
606         Break
607     EndIf
608     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
609     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
610     If M_20# = MContinue% Then GoTo *RE_PLT_GET
611     *CompPltGet1
612 '
613 '    Mov PProductOnPltGet_1      '製品上空(位置変更1/14中村)
614 '
615 '    Wait M_In(11262) = 1        '位置決め出端センサーON
616     MRtn = frInCheck(11262,1,MSETTIMEOUT05&)   '位置決め出端センサーON
617     If MRtn = 1 Then GoTo *CompPltGet2
618     fErrorProcess(11,231,282,0)
619     If M_20# = MNext% Then M_20# = MClear%
620     If M_20# = MAbout% Or M_20# = MNgProcess% Then
621         M_Out(12268) = 0            '位置決め
622         M_Out(12269) = 1            '位置決め戻ON
623         Mov PProductOnPltGet_2
624         Mov PInitialPosition'退避ルート
625         Break
626     EndIf
627     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
628     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
629     If M_20# = MContinue% Then GoTo *RE_PLT_GET
630     *CompPltGet2
631 '
632     Mov PProductOnPltGet_1      '製品上空
633     M_Out(12268) = 0            '位置決め出OFF
634     M_Out(12269) = 1            '位置決め戻ON
635 '    Wait M_In(11263) = 1        '位置決め戻端センサーON
636     MRtn = frInCheck(11263,1,MSETTIMEOUT05&)   '位置決め戻り端センサー
637     If MRtn = 1 Then GoTo *CompPltGet3
638     fErrorProcess(11,234,284,0)
639     If M_20# = MNext% Then M_20# = MClear%
640     If M_20# = MAbout% Or M_20# = MNgProcess% Then
641         Mov PProductOnPltGet_2      '退避ルート
642         Mov PInitialPosition
643         Break
644     EndIf
645     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
646     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
647     If M_20# = MContinue% Then GoTo *RE_PLT_GET
648     *CompPltGet3
649 '
650     Ovrd 30
651     Mvs PProductOnPltGet        '製品を取る位置
652     M_Out(12257) = 0            'チャック開OFF
653     M_Out(12256) = 1            'チャック閉ON
654 '
655 '    Wait M_In(11266) = 1        'チャック閉センサーON
656     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'チャック閉センサーON
657     If MRtn = 1 Then GoTo *CompPltGet4
658     M_Out(12256) = 0        '退避ルート
659     M_Out(12257) = 1
660     Dly 2.0
661     Mvs PProductOnPltGet_1
662     Mov PProductOnPltGet_2
663     M_Out(12257) = 0
664     fErrorProcess(11,245,284,0)
665     If M_20# = MNext% Then M_20# = MClear%
666     If M_20# = MAbout% Or M_20# = MNgProcess% Then
667         Mov PInitialPosition
668         Break
669     EndIf
670     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
671     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
672     Mvs PProductOnPltGet_1
673     Mvs PProductOnPltGet
674     Dly 0.1
675     M_Out(12257) = 0            'チャック開OFF
676     M_Out(12256) = 1            'チャック閉ON
677     If M_20# = MContinue% Then GoTo *RE_PLT_GET
678     Mvs PProductOnPltGet
679     Dly 0.1
680     M_Out(12257) = 0            'チャック開OFF
681     M_Out(12256) = 1            'チャック閉ON
682     Dly 2.0
683     *CompPltGet4
684 '
685 '    Wait M_In(11264) = 1        '製品検出センサーON
686     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '製品検出センサーセンサーON
687     If MRtn = 1 Then GoTo *CompPltGet5
688     fErrorProcess(11,252,284,0)
689     If M_20# = MNext% Then M_20# = MClear%
690     If M_20# = MAbout% Or M_20# = MNgProcess% Then
691         M_Out(12256) = 0        '退避ルート
692         M_Out(12257) = 1
693         Mvs PProductOnPltGet_1
694         Mov PProductOnPltGet_2
695         Mov PInitialPosition
696         Break
697     EndIf
698     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
699     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
700     If M_20# = MContinue% Then GoTo *RE_PLT_GET
701     *CompPltGet5
702 '
703     MRtn = FnCtlValue2(1)       '投入数＋１  2022/04/28 渡辺
704     Mvs PProductOnPltGet_1      '製品上空
705     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
706     Ovrd 60
707     Mov PProductOnPltGet_2      '製品上空回避点
708     '
709     '製品を治具に置く(点の追加,またそれによる変数名変更(9/2中村))
710     Fine 1.0 , P
711     Mov PProductOnJigSet_4      'パレット側回避点(末尾3から4(9/2中村))
712     Fine 0 , P
713     Ovrd 100
714     Mov PProductOnJigSet_3      'パレット-治具中間点(末尾2から3(9/2中村))
715     Mov PProductOnJigSet_2      '治具側回避点(点の追加(9/2中村))
716     '
717     *RE_JIG_SET_1
718     '
719     M_Out(12259) = 0            '治具製品チャック閉OFF
720     M_Out(12258) = 1            '治具製品チャック開ON
721     M_Out(12261)=0              '治具製品クランパー引込端OFF
722     M_Out(12260)=1              '治具製品クランパー出端ON
723 '
724 '    Wait M_In(11270) = 1        '治具製品チャック開センサーON
725     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   '治具製品チャック開センサーON
726     If MRtn = 1 Then GoTo *CompJIGSet1
727     fErrorProcess(11,259,284,0)
728     If M_20# = MNext% Then M_20# = MClear%
729     If M_20# = MAbout% Or M_20# = MNgProcess% Then
730         Mvs PProductOnJigSet_2      '退避ルート
731         Mov PProductOnJigSet_3
732         Mov PProductOnJigSet_4
733         Mov PProductOnPltSet_2
734         Mov PProductOnPltSet_1
735         Ovrd 25
736         Mvs PProductOnPltSet
737         Dly 0.3
738         M_Out(12256)=0              'チャック閉OFF
739         M_Out(12257)=1              'チャック開ON
740         Dly 0.5
741         Mvs PProductOnPltSet_1
742         Ovrd 100
743         Mov PProductOnPltSet_2
744         Mov PInitialPosition
745         Break
746     EndIf
747     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
748     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
749     If M_20# = MContinue% Then GoTo *RE_JIG_SET_1
750     *CompJIGSet1
751 '
752 '    Wait M_In(11272) = 1        'ハンド-製品クランパー開放端センサーON
753     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   'ハンド-製品クランパー開放端センサーON
754     If MRtn = 1 Then GoTo *CompJIGSet2
755     fErrorProcess(11,257,284,0)
756     If M_20# = MNext% Then M_20# = MClear%
757     If M_20# = MAbout% Or M_20# = MNgProcess% Then
758         Mvs PProductOnJigSet_2      '退避ルート
759         Mov PProductOnJigSet_3
760         Mov PProductOnJigSet_4
761         Mov PProductOnPltSet_2
762         Mov PProductOnPltSet_1
763         Ovrd 25
764         Mvs PProductOnPltSet
765         Dly 0.3
766         M_Out(12256)=0              'チャック閉OFF
767         M_Out(12257)=1              'チャック開ON
768         Dly 2.0
769         Mvs PProductOnPltSet_1
770         Ovrd 100
771         Mov PProductOnPltSet_2
772         Mov PInitialPosition
773         Break
774     EndIf
775     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
776     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
777     If M_20# = MContinue% Then GoTo *RE_JIG_SET_1
778     *CompJIGSet2
779 '
780     Mvs PProductOnJigSet_1      '治具上空
781     Ovrd 30
782     Mvs PProductOnJigSet        '製品置き位置
783     '
784     *RE_JIG_SET_2
785     '
786     M_Out(12258) = 0            '治具製品チャック開OFF
787     M_Out(12259) = 1            '治具製品チャック閉ON
788     '
789 '    Wait M_In(11271) = 1
790     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)
791     If MRtn = 1 Then GoTo *CompJIGSet3
792     fErrorProcess(11,258,284,0)
793     If M_20# = MNext% Then M_20# = MClear%
794     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
795         M_Out(12256)=0              'チャック閉OFF
796         M_Out(12257)=1              'チャック開ON
797         Dly 2.0
798         Mvs PProductOnJigSet_1
799         Mvs PProductOnJigSet_2
800         Mov PProductOnJigSet_3
801         Mov PProductOnJigSet_4
802         Mov PInitialPosition
803         Break
804     EndIf
805     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
806     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
807     If M_20# = MContinue% Then GoTo *RE_JIG_SET_2
808     *CompJIGSet3
809     '
810     M_Out(12260)=0              '製品クランパー出端OFF
811     M_Out(12261)=1              '製品クランパー引込端ON
812 '    Wait M_In(11273)=1          '製品クランパー引込端センサーON
813     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '治具製品クランパー引込端センサーON
814     If MRtn = 1 Then GoTo *CompJIGSet4
815     fErrorProcess(11,256,284,0)
816     If M_20# = MNext% Then M_20# = MClear%
817     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
818         M_Out(12256)=0              'チャック閉OFF
819         M_Out(12257)=1              'チャック開ON
820         Dly 2.0
821         Mvs PProductOnJigSet_1
822         Mvs PProductOnJigSet_2
823         Mov PProductOnJigSet_3
824         Mov PProductOnJigSet_4
825         Mov PInitialPosition
826         Break
827     EndIf
828     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
829     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
830     If M_20# = MContinue% Then GoTo *RE_JIG_SET_2
831     *CompJIGSet4
832     '
833     M_Out(12256)=0              'チャック閉OFF
834     M_Out(12257)=1              'チャック開ON
835 '    Wait M_In(11265)=1          'チャック開センサーON
836     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'チャック開センサーON
837     If MRtn = 1 Then GoTo *CompJIGSet5
838     fErrorProcess(11,244,284,0)
839     If M_20# = MNext% Then M_20# = MClear%
840     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
841         Mvs PProductOnJigSet_1
842         Mvs PProductOnJigSet_2
843         Mov PProductOnJigSet_3
844         Mov PProductOnJigSet_4
845         Mov PInitialPosition
846         Break
847     EndIf
848     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
849     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
850     If M_20# = MContinue% Then GoTo *RE_JIG_SET_2
851     *CompJIGSet5
852     '
853     Mvs PProductOnJigSet_1      '治具上空
854     Mvs PProductOnJigSet_2      '治具側回避点(点の追加(9／2中村))
855     '
856     *RE_JIG_SET_3
857     '
858 '    Wait M_In(11264)=0          '製品検出センサーOFF
859     MRtn = frInCheck(11264,0,MSETTIMEOUT05&)   '製品検出センサーOFF
860     If MRtn = 1 Then GoTo *CompJIGSet6
861     fErrorProcess(11,253,284,0)
862     If M_20# = MNext% Then M_20# = MClear%
863     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
864         Mvs PProductOnJigSet_1
865         Mvs PProductOnJigSet_2
866         Mov PProductOnJigSet_3
867         Mov PProductOnJigSet_4
868         Mov PInitialPosition
869         Break
870     EndIf
871     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
872     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
873     If M_20# = MContinue% Then GoTo *RE_JIG_SET_3
874     *CompJIGSet6
875     '
876     Ovrd 100
877     '
878 'ねじ締め順変更ここから(6/9中村)
879     *RE_SCREW_SET_1
880     '
881     M_Out(12257) = 0            'チャック開OFF(ネジピックアップ時干渉対策9/10中村)
882     M_Out(12256) = 1            'チャック閉ON(ネジピックアップ時干渉対策9/10中村)
883 '    Wait M_In(11266) = 1        'チャック閉センサーON
884     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    'チャック閉センサーON
885     If MRtn = 1 Then GoTo *CompScrewSet1
886     fErrorProcess(11,245,284,0)
887     If M_20# = MNext% Then M_20# = MClear%
888     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
889         Mvs PProductOnJigSet_1
890         Mvs PProductOnJigSet_2
891         Mov PProductOnJigSet_3
892         Mov PProductOnJigSet_4
893         Mov PInitialPosition
894         Break
895     EndIf
896     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
897     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
898     If M_20# = MContinue% Then GoTo *RE_SCREW_SET_1
899     *CompScrewSet1
900 'ハンドの向きをHSねじ締めの向きに変更する座標が必要(6/9中村)
901     Mov PScrewSupplyHS_5        'ハンド向き変え
902     Mov PScrewSupplyHS_3        '治具側ねじロボ回避点
903     'ヒートシンクのネジ締め
904     '
905     'ヒートシンク用ネジ供給機へネジを取りに行く
906     'GoSub *ScrewSupplyHS         'コメントアウト(9/3中村)
907     '*ScrewSupplyHS               'コメントアウト
908     PGetScrewPos(1) = PScrewSupplyHS_1  'ネジピックアップ上空
909     PGetScrewPos(2) = PScrewSupplyHS_2  'ネジ供給機回避点
910     PGetScrewPos(9) = PScrewSupplyHS_4  '吸着不良捨て位置
911     PGetScrewPos(10) = PScrewSupplyHS   'ネジピックアップ位置
912 '    Mov PScrewSupplyHS_2          'ネジ供給機回避点(9/30以下5行コメントアウト(中村))
913 '    Mvs PScrewSupplyHS_1          'ネジピックアップ上空(MovからMvsへ変更(9/3中村))
914 '    Mvs PScrewSupplyHS            'ネジピックアップ
915 '    Mvs PScrewSupplyHS_1          'ネジピックアップ上空
916 '    Mvs PScrewSupplyHS_2          'ネジ供給機回避点(MovからMvsへ変更(9/3中村))
917     *RE_SCREW_GET_1
918     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
919     If MRtn = 1 Then GoTo *CompScrewGet1
920     If M_20# = MNext% Then M_20# = MClear%
921     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
922         Mov PEscapePosition_4
923         Mov PEscapePosition_2
924         Mov PEscapePosition
925         Mov PInitialPosition
926         Break
927     EndIf
928     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
929     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
930     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
931     *CompScrewGet1
932     '
933     Mov PScrewSupplyHS_3          '治具側ねじロボ回避点
934     'Return                       'コメントアウト(9/3中村)
935     '
936     '①番ネジ締め
937 '    Mov PScrewHeatSink1_1         '①上空(以下5行コメントアウト(9/30中村))
938 '    Ovrd 5
939 '    Mvs PScrewHeatSink1           '①ネジ着座
940 '    Ovrd 10
941 '    Mvs PScrewHeatSink1_1         '①上空
942     PScrewPos(1) = PScrewHeatSink1_1    'ねじ締め上空(以下4行追加(9/30中村))
943     PScrewPos(2) = PScrewHeatSink1_0    'ねじ締め開始位置
944     PScrewPos(10) = PScrewHeatSink1     'ねじ締め終了位置
945     M_Out16(12672) = 1              'ネジ締め位置番号送信
946     MRtn = ScrewTight(PScrewPos,2,4.914)          'ねじ締め開始
947     M_Out16(12672) = 0              'ネジ締め位置番号クリア
948     If MRtn = 1 Then GoTo *CompScrew1
949     Mov PScrewSupplyHS_3
950     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
951     MScrewErrorCord% = MScrewErrorCord% + 1
952     fErrorProcess(11,MScrewErrorCord%,52,0)
953 '    fErrorProcess(11,57,52,0)
954     If M_20# = MNext% Then M_20# = MClear%
955     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
956         Mov PEscapePosition_3
957         Mov PEscapePosition_2
958         Mov PEscapePosition
959         Mov PInitialPosition
960         Break
961     EndIf
962     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
963     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
964     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_1
965     *CompScrew1
966     '
967     'ヒートシンク用ネジ供給機へネジを取りに行く
968     'GoSub *ScrewSupplyHS         'コメントアウト(9/10中村)
969     Mov PScrewSupplyHS_3          '治具側ねじロボ回避点(以下3行追加(9/30中村))
970     *RE_SCREW_GET_2
971     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        'ネジ受け取り開始
972     If MRtn = 1 Then GoTo *CompScrewGet2
973     If M_20# = MNext% Then M_20# = MClear%
974     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
975         Mov PEscapePosition_4
976         Mov PEscapePosition_2
977         Mov PEscapePosition
978         Mov PInitialPosition
979         Break
980     EndIf
981     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
982     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
983     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
984     *CompScrewGet2
985     '
986     Mov PScrewSupplyHS_3          '治具側ねじロボ回避点
987     '
988     '②番ネジ締め
989 '    Mov PScrewHeatSink2_1         '②上空(以下5行コメントアウト(9/30中村))
990 '    Ovrd 5
991 '    Mvs PScrewHeatSink2           '②ネジ着座
992 '    Ovrd 10
993 '    Mvs PScrewHeatSink2_1         '②上空
994     PScrewPos(1) = PScrewHeatSink2_1    'ねじ締め上空(以下4行追加(9/30中村))
995     PScrewPos(2) = PScrewHeatSink2_0    'ねじ締め開始位置
996     PScrewPos(10) = PScrewHeatSink2     'ねじ締め終了位置
997     M_Out16(12672) = 2              'ネジ締め位置番号送信
998     MRtn = ScrewTight(PScrewPos,6,4.914)          'ねじ締め開始
999     M_Out16(12672) = 0              'ネジ締め位置番号クリア
1000     If MRtn = 1 Then GoTo *CompScrew2
1001     Mov PScrewSupplyHS_3
1002     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
1003     MScrewErrorCord% = MScrewErrorCord% + 2
1004     fErrorProcess(11,MScrewErrorCord%,52,0)
1005 '    fErrorProcess(11,58,52,0)
1006     If M_20# = MNext% Then M_20# = MClear%
1007     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1008         Mov PEscapePosition_3
1009         Mov PEscapePosition_2
1010         Mov PEscapePosition
1011         Mov PInitialPosition
1012         Break
1013     EndIf
1014     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1015     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1016     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_2
1017     *CompScrew2
1018     '
1019     '
1020     '
1021     '
1022     Mov PScrewSupplyPlate_3          'ネジピックアップ向き合わせ位置
1023     '
1024 '    *RE_SCREW_SET_2
1025     '
1026     '治具左上向きにて90度回転(並列処理化4/13中村)
1027 '    M_Out(12262) = 0             '回転ストッパー出OFF
1028 '    M_Out(12263) = 1             '回転ストッパー戻ON
1029 '    '
1030 ''    Wait M_In(11275) = 1         '回転ストッパー戻端検出センサーON
1031 '    MRtn = frInCheck(11275,1,MSETTIMEOUT05&)   '位置決め戻端センサーON
1032 '    If MRtn = 1 Then GoTo *CompScrewSet2
1033 '    fErrorProcess(11,262,284,0)
1034 '    If M_20# = MNext% Then M_20# = MClear%
1035 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1036 '        Mov PEscapePosition_3
1037 '        Mov PEscapePosition_2
1038 '        Mov PEscapePosition
1039 '        Mov PInitialPosition
1040 '        Break
1041 '    EndIf
1042 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1043 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1044 '    If M_20# = MContinue% Then GoTo *RE_SCREW_SET_2
1045 '    *CompScrewSet2
1046     M_25# = 1                   '並列処理：CW回転開始(デバッグ中)
1047 '
1048     '左側面のネジ締め
1049     '
1050     '側板用ネジ供給機へネジを取りに行く
1051     'GoSub *ScrewSupplyPlate
1052     '*ScrewSupplyPlate
1053     PGetScrewPos(1) = PScrewSupplyPlate_1  'ネジピックアップ上空
1054     PGetScrewPos(2) = PScrewSupplyPlate_2  'ネジ供給機回避点
1055     PGetScrewPos(9) = PScrewSupplyPlate_7  '吸着不良戻し位置
1056     PGetScrewPos(10) = PScrewSupplyPlate   'ネジピックアップ位置
1057 *RE_SCREW_GET_3
1058     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        'ネジ受け取り開始
1059 '
1060     If MRtn = 1 Then GoTo *CompScrewGet3
1061     If M_20# = MNext% Then M_20# = MClear%
1062     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1063         Mov PEscapePosition_3
1064         Mov PEscapePosition_2
1065         Mov PEscapePosition
1066         Mov PInitialPosition
1067         Break
1068     EndIf
1069     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1070     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1071     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
1072     *CompScrewGet3
1073     '
1074     Mov PScrewSupplyPlate_3          'ネジ供給機-治具間中間点(追加9/10中村)
1075     'タクト短縮のため追加、修正2/3中村
1076 '    *RE_CW_ROT
1077 '    If M_20# = MContinue% Then
1078 '        M_Out(12264) = 1 Dly 0.3     'CW1バルブON
1079 '    EndIf
1080 '    MRtn = frInCheck(11278,1,MSETTIMEOUT05&)   'CW端センサーON
1081 '    If MRtn = 1 Then GoTo *CompScrewSet3
1082 '    fErrorProcess(11,264,284,0)
1083 '    If M_20# = MNext% Then M_20# = MClear%
1084 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1085     M_20# = MClear%
1086 *LOOP1
1087     If M_20# = MContinue% Then
1088         M_25# = 1
1089         M_20# = MClear%
1090     EndIf
1091     If M_26# <> 0 Then GoTo *LOOP1END Else GoTo *LOOP1
1092 *LOOP1END
1093 '
1094     If M_26# = 1 Then GoTo *Set1End                            '正常終了ならエラー処理なし
1095     If M_In(11275) = 1 And M_In(11278) = 1 Then GoTo *Set1End  '今現在が正常終了と同じセンサー状態ならエラー処理なし
1096     If M_In(11275) = 0 Then                                    'センサーの状態を見てエラーを出す
1097         fErrorProcess(11,262,284,0)
1098     ElseIf M_In(11278) = 0 Then
1099         fErrorProcess(11,264,284,0)
1100     EndIf
1101     M_26# = 0
1102     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1103         Mov PEscapePosition_3
1104         Mov PScrewSupplyPlate_7  '吸着不良戻し位置
1105         M_Out(12249)=1           'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1106         Dly 0.2
1107         '破壊ON
1108         M_Out(Y6B_VB1)=1 '真空破壊ON
1109         'ビット回転
1110         M_Out(Y61_Driver)=1
1111         Dly 0.5
1112         '                '
1113         Ovrd 100
1114         JOvrd M_NJovrd
1115         Spd M_NSpd
1116         'ドライバーを上下させねじを振り落とす
1117         Mov PScrewSupplyPlate_7,10
1118         Mov PScrewSupplyPlate_7
1119         Dly 0.1
1120         Mov PScrewSupplyPlate_7,10
1121         Mov PScrewSupplyPlate_7
1122 '
1123         'ネジ落ち待ち
1124         Wait M_In(11268) = 0
1125         'ビット回転停止
1126         M_Out(Y61_Driver)=0
1127         Dly 0.1
1128         '破壊OFF
1129         M_Out(Y6B_VB1)=0 '真空破壊OFF
1130         M_Out(12249)=0                  'ねじ吸着　OFF
1131         Mov PEscapePosition_2
1132         Mov PEscapePosition
1133         Mov PInitialPosition
1134         Break
1135     EndIf
1136     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1137     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1138 '    If M_20# = MContinue% Then GoTo *RE_CW_ROT
1139 '    *CompScrewSet3
1140     If M_20# = MNext% Then M_20# = MClear%
1141     If M_20# = MContinue% Then GoTo *LOOP1
1142 '
1143 *Set1End
1144 '    M_26# = 0                       '初期化タイミング変更6/9中村
1145 '
1146     Mov PScrewSupplyPlate_4          '治具側回避点
1147     'Return                          'コメントアウト(9/3中村)
1148     '
1149     '側板Lネジ締め(仕様変更につきPScrewPlateL1をPScrewPlateLへ変更)
1150 '    Mov PScrewPlateL1_1            '①上空(以下5行コメントアウト(9/30中村))
1151 '    Ovrd 5
1152 '    Mvs PScrewPlateL1              '①ネジ着座
1153 '    Ovrd 10
1154 '    Mvs PScrewPlateL1_1            '①上空
1155     PScrewPos(1) = PScrewPlateL1_1    'ねじ締め上空(以下4行追加(9/30中村))
1156     PScrewPos(2) = PScrewPlateL1_0    'ねじ締め開始位置
1157     PScrewPos(10) = PScrewPlateL1     'ねじ締め終了位置
1158     M_Out16(12672) = 3              'ネジ締め位置番号送信
1159     MRtn = ScrewTight(PScrewPos,4,4.914)          'ねじ締め開始
1160     M_Out16(12672) = 0              'ネジ締め位置番号クリア
1161     If MRtn = 1 Then GoTo *CompScrew3
1162     Mov PScrewSupplyPlate_4
1163     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
1164     MScrewErrorCord% = MScrewErrorCord% + 3
1165     fErrorProcess(11,MScrewErrorCord%,52,0)
1166 '    fErrorProcess(11,53,52,0)
1167     If M_20# = MNext% Then M_20# = MClear%
1168     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1169         Mov PEscapePosition_3
1170         Mov PEscapePosition_2
1171         Mov PEscapePosition
1172         Mov PInitialPosition
1173         Break
1174     EndIf
1175     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1176     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1177     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_3
1178     *CompScrew3
1179     M_26# = 0                       '初期化タイミング変更6/9中村
1180 '
1181 '    '以下17行仕様変更につきコメントアウト(11/4中村)
1182 '    '側板用ネジ供給機へネジを取りに行く
1183 '    'GoSub *ScrewSupplyPlate       'コメントアウト(9/9中村)
1184     Mov PScrewSupplyPlate_4          '治具側回避点(以下5行追加(9/30中村))
1185     Mov PScrewSupplyPlate_3          '治具-ネジ供給機間中間点
1186     *RE_SCREW_GET_4
1187     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        'ネジ受け取り開始
1188     If MRtn = 1 Then GoTo *CompScrewGet4
1189     If M_20# = MNext% Then M_20# = MClear%
1190     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1191         Mov PEscapePosition_3
1192         Mov PEscapePosition_2
1193         Mov PEscapePosition
1194         Mov PInitialPosition
1195         Break
1196     EndIf
1197     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1198     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1199     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
1200     *CompScrewGet4
1201     '
1202     Mov PScrewSupplyPlate_3          'ネジ供給機-治具間中間点
1203     Mov PScrewSupplyPlate_4          '治具側回避点
1204 '    '
1205 '    '②番ネジ締め
1206 ''    Mov PScrewPlateL2_1            '②上空(以下5行コメントアウト(9/30中村))
1207 ''    Ovrd 5
1208 ''    Mvs PScrewPlateL2              '②ネジ着座
1209 ''    Ovrd 10
1210 ''    Mvs PScrewPlateL2_1            '②上空
1211     PScrewPos(1) = PScrewPlateL2_1    'ねじ締め上空(以下4行追加(9/30中村))
1212     PScrewPos(2) = PScrewPlateL2_0    'ねじ締め開始位置
1213     PScrewPos(10) = PScrewPlateL2     'ねじ締め終了位置
1214     M_Out16(12672) = 4              'ネジ締め位置番号送信
1215     MRtn = ScrewTight(PScrewPos,4,4.914)          'ねじ締め開始
1216     M_Out16(12672) = 0              'ネジ締め位置番号クリア
1217     If MRtn = 1 Then GoTo *CompScrew4
1218     Mov PScrewSupplyPlate_4
1219     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
1220     MScrewErrorCord% = MScrewErrorCord% + 4
1221     fErrorProcess(11,MScrewErrorCord%,52,0)
1222 '    fErrorProcess(11,54,52,0)
1223     If M_20# = MNext% Then M_20# = MClear%
1224     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1225         Mov PEscapePosition_3
1226         Mov PEscapePosition_2
1227         Mov PEscapePosition
1228         Mov PInitialPosition
1229         Break
1230     EndIf
1231     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1232     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1233     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_4
1234     *CompScrew4
1235 '
1236     Mov PScrewSupplyPlate_4        '治具回転につきあらかじめ回避させる(追加9/10中村)
1237     '
1238     '
1239 '    *RE_SCREW_SET_3
1240 '    '
1241 '    M_Out(12265) = 1 Dly 0.3       'CW2バルブON
1242 ''    Wait M_In(11277) = 1           'CCWセンター検出センサーON
1243 '    MRtn = frInCheck(11277,1,MSETTIMEOUT05&)
1244 '    If MRtn = 1 Then GoTo *CompScrewSet4
1245 '    fErrorProcess(11,265,284,0)
1246 '    If M_20# = MNext% Then M_20# = MClear%
1247 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1248 '        Mov PEscapePosition_3
1249 '        Mov PEscapePosition_2
1250 '        Mov PEscapePosition
1251 '        Mov PInitialPosition
1252 '        Break
1253 '    EndIf
1254 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1255 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1256 '    If M_20# = MContinue% Then GoTo *RE_SCREW_SET_3
1257 '    *CompScrewSet4
1258 '    '
1259 '    *RE_SCREW_SET_4
1260 '    '
1261 '    M_Out(12266) = 1 Dly 0.3       'CCW1バルブON
1262 '    Wait M_In(11279) = 1           'CCW端検出センサーON
1263 '    MRtn = frInCheck(11279,1,MSETTIMEOUT05&)   'CCW端検出センサーON(タクト短縮のためコメントアウト2/3中村)
1264 '    If MRtn = 1 Then GoTo *CompScrewSet5
1265 '    fErrorProcess(11,266,284,0)
1266 '    If M_20# = MNext% Then M_20# = MClear%
1267 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1268 '        Mov PEscapePosition_3
1269 '        Mov PEscapePosition_2
1270 '        Mov PEscapePosition
1271 '        Mov PInitialPosition
1272 '        Break
1273 '    EndIf
1274 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1275 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1276 '    If M_20# = MContinue% Then GoTo *RE_SCREW_SET_4
1277 '    *CompScrewSet5
1278     '
1279     '右側面のネジ締め
1280     '
1281     '側板用ネジ供給機へネジを取りに行く
1282     'GoSub *ScrewSupplyPlate       'コメントアウト(9/9中村)
1283     Mov PScrewSupplyPlate_3          '治具-ネジ供給機間中間点(以下4行追加(9/30中村))
1284     '治具180度回転
1285     M_25# = 2
1286     *RE_SCREW_GET_5
1287     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        'ネジ受け取り開始
1288     If MRtn = 1 Then GoTo *CompScrewGet5
1289     If M_20# = MNext% Then M_20# = MClear%
1290     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1291         Mov PEscapePosition_3
1292         Mov PEscapePosition_2
1293         Mov PEscapePosition
1294         Mov PInitialPosition
1295         Break
1296     EndIf
1297     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1298     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1299     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
1300     *CompScrewGet5
1301     Mov PScrewSupplyPlate_3          'ネジ供給機-治具間中間点
1302 '
1303 '    *RE_CCW_ROT
1304 '    If M_20# = MContinue% Then
1305 '        M_Out(12266) = 1 Dly 0.3     'CW1バルブON
1306 '    EndIf
1307 '    'CCWセンサー検出(タクト短縮のため移動、修正）
1308 '    MRtn = frInCheck(11279,1,MSETTIMEOUT05&)   'CCW端検出センサーON
1309 '    If MRtn = 1 Then GoTo *CompScrewSet5
1310 '    fErrorProcess(11,266,284,0)
1311 '    If M_20# = MNext% Then M_20# = MClear%
1312     M_20# = MClear%
1313 *LOOP2
1314     If M_20# = MContinue% Then
1315         M_25# = 2
1316         M_20# = MClear%
1317     EndIf
1318     If M_26# <> 0 Then GoTo *LOOP2END Else GoTo *LOOP2
1319 *LOOP2END
1320 '
1321     If M_26# = 1 Then GoTo *Set2End                            '正常終了ならエラー処理なし
1322     If M_In(11279) = 1 Then GoTo *Set2End  '今現在が正常終了と同じセンサー状態ならエラー処理なし
1323     If M_In(11277) = 0 Then                                    'センサーの状態を見てエラーを出す
1324         fErrorProcess(11,265,284,0)
1325     ElseIf M_In(11279) = 0 Then
1326         fErrorProcess(11,266,284,0)
1327     EndIf
1328     M_26# = 0
1329     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1330         Mov PEscapePosition_3
1331         Mov PScrewSupplyPlate_7  '吸着不良戻し位置
1332         M_Out(12249)=1       'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
1333         Dly 0.2
1334         '破壊ON
1335         M_Out(Y6B_VB1)=1 '真空破壊ON
1336         'ビット回転
1337         M_Out(Y61_Driver)=1
1338         Dly 0.5
1339         '                '
1340         Ovrd 100
1341         JOvrd M_NJovrd
1342         Spd M_NSpd
1343         'ドライバーを上下させねじを振り落とす
1344         Mov PScrewSupplyPlate_7,10
1345         Mov PScrewSupplyPlate_7
1346         Dly 0.1
1347         Mov PScrewSupplyPlate_7,10
1348         Mov PScrewSupplyPlate_7
1349 '
1350         'ネジ落ち待ち
1351         Wait M_In(11268) = 0
1352         'ビット回転停止
1353         M_Out(Y61_Driver)=0
1354         Dly 0.1
1355         '破壊OFF
1356         M_Out(Y6B_VB1)=0 '真空破壊OFF
1357         M_Out(12249)=0                  'ねじ吸着　OFF
1358         Mov PEscapePosition_2
1359         Mov PEscapePosition
1360         Mov PInitialPosition
1361         Break
1362     EndIf
1363     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1364     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1365 '    If M_20# = MContinue% Then GoTo *RE_CCW_ROT
1366     If M_20# = MNext% Then M_20# = MClear%
1367     If M_20# = MContinue% Then GoTo *LOOP2
1368 '    *CompScrewSet5
1369 *Set2End
1370 '    M_26# = 0                       '初期化タイミング変更6/9中村
1371 '
1372     Mov PScrewSupplyPlate_4          '治具側回避点
1373     '
1374     '①番ネジ締め(仕様変更につきPScrewPlateR1をPScrewPlateRへ変更)
1375 '    Mov PScrewPlateR1_1            '①上空(以下5行コメントアウト(9/30中村))
1376 '    Ovrd 5
1377 '    Mvs PScrewPlateR1              '①ネジ着座
1378 '    Ovrd 10
1379 '    Mvs PScrewPlateR1_1            '①上空
1380     PScrewPos(1) = PScrewPlateR1_1    'ねじ締め上空(以下4行追加(9/30中村))
1381     PScrewPos(2) = PScrewPlateR1_0    'ねじ締め開始位置
1382     PScrewPos(10) = PScrewPlateR1     'ねじ締め終了位置
1383     M_Out16(12672) = 5              'ネジ締め位置番号送信
1384     MRtn = ScrewTight(PScrewPos,4,4.914)          'ねじ締め開始
1385     M_Out16(12672) = 0              'ネジ締め位置番号クリア
1386     If MRtn = 1 Then GoTo *CompScrew5
1387     Mov PScrewSupplyPlate_4
1388     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
1389     MScrewErrorCord% = MScrewErrorCord% + 5
1390     fErrorProcess(11,MScrewErrorCord%,52,0)
1391 '    fErrorProcess(11,55,52,0)
1392     If M_20# = MNext% Then M_20# = MClear%
1393     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1394         Mov PEscapePosition_3
1395         Mov PEscapePosition_2
1396         Mov PEscapePosition
1397         Mov PInitialPosition
1398         Break
1399     EndIf
1400     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1401     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1402     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_5
1403     *CompScrew5
1404     M_26# = 0                       '初期化タイミング変更6/9中村
1405 '
1406 '
1407     '以下18行仕様変更につきコメントアウト(11/4中村)
1408 '    '側板用ネジ供給機へネジを取りに行く
1409 '    'GoSub *ScrewSupplyPlate       'コメントアウト(9/9中村)
1410     Mov PScrewSupplyPlate_4          '治具側回避点(以下5行追加(9/30中村))
1411     Mov PScrewSupplyPlate_3          '治具-ネジ供給機間中間点
1412     '
1413     *RE_SCREW_GET_6
1414     '
1415     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        'ネジ受け取り開始
1416     If MRtn = 1 Then GoTo *CompScrewGet6
1417     If M_20# = MNext% Then M_20# = MClear%
1418     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1419         Mov PEscapePosition_3
1420         Mov PEscapePosition_2
1421         Mov PEscapePosition
1422         Mov PInitialPosition
1423         Break
1424     EndIf
1425     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1426     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1427     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
1428     *CompScrewGet6
1429     Mov PScrewSupplyPlate_3          'ネジ供給機-治具間中間点
1430     Mov PScrewSupplyPlate_4          '治具側回避点
1431 '    '
1432 '    '②番ネジ締め
1433 ''    Mov PScrewPlateR2_1            '②上空(以下5行コメントアウト(9/30中村))
1434 ''    Ovrd 5
1435 ''    Mvs PScrewPlateR2              '②ネジ着座
1436 ''    Ovrd 10
1437 ''    Mvs PScrewPlateR2_1            '②上空
1438     PScrewPos(1) = PScrewPlateR2_1    'ねじ締め上空(以下4行追加(9/30中村))
1439     PScrewPos(2) = PScrewPlateR2_0    'ねじ締め開始位置
1440     PScrewPos(10) = PScrewPlateR2     'ねじ締め終了位置
1441     M_Out16(12672) = 6              'ネジ締め位置番号送信
1442     MRtn = ScrewTight(PScrewPos,4,4.914)          'ねじ締め開始
1443     M_Out16(12672) = 0              'ネジ締め位置番号クリア
1444     If MRtn = 1 Then GoTo *CompScrew6
1445     Mov PScrewSupplyPlate_4
1446     MScrewErrorCord% = FnScreEroorCord()         'エラーコメントに電ドラエラーコード追加 22.05.23 渡辺
1447     MScrewErrorCord% = MScrewErrorCord% + 6
1448     fErrorProcess(11,MScrewErrorCord%,52,0)
1449 '    fErrorProcess(11,56,52,0)
1450     If M_20# = MNext% Then M_20# = MClear%
1451     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1452         Mov PEscapePosition_3
1453         Mov PEscapePosition_2
1454         Mov PEscapePosition
1455         Mov PInitialPosition
1456         Break
1457     EndIf
1458     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1459     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1460     If M_20# = MContinue% Then GoTo *RE_SCREW_GET_6
1461     *CompScrew6
1462     '
1463     Mov PScrewSupplyHS_3           '治具側ねじロボ回避点
1464     '
1465     '治具90度回転
1466 '    *RE_CENTER_POS
1467 '    M_Out(12267) = 1 Dly 0.3       'CCW2バルブON
1468 ''    Wait M_In(11276) = 1           'CWセンター検出
1469 '    MRtn = frInCheck(11276,1,MSETTIMEOUT05&)   'CWセンター検出
1470 '    If MRtn = 1 Then GoTo *CompSenterPos1
1471 '    fErrorProcess(11,265,284,0)
1472 '    If M_20# = MNext% Then M_20# = MClear%
1473 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1474 '        Mov PEscapePosition_2
1475 '        Mov PEscapePosition
1476 '        Mov PInitialPosition
1477 '        Break
1478 '    EndIf
1479 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1480 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1481 '    If M_20# = MContinue% Then GoTo *RE_CENTER_POS
1482 '    *CompSenterPos1
1483 '    '
1484 '    M_Out(12263) = 0               '回転ストッパー戻OFF
1485 '    M_Out(12262) = 1               '回転ストッパー出ON
1486 '    '
1487 ''    Wait M_In(11274) = 1           '回転ストッパー出端検出ON
1488 '    MRtn = frInCheck(11274,1,MSETTIMEOUT05&)   '回転ストッパー出端検出ON
1489 '    If MRtn = 1 Then GoTo *CompSenterPos2
1490 '    fErrorProcess(11,263,284,0)
1491 '    If M_20# = MNext% Then M_20# = MClear%
1492 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1493 '        Mov PEscapePosition_2
1494 '        Mov PEscapePosition
1495 '        Mov PInitialPosition
1496 '        Break
1497 '    EndIf
1498 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1499 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1500 '    If M_20# = MContinue% Then GoTo *RE_CENTER_POS
1501 '    *CompSenterPos2
1502 '    '
1503 '    '
1504 '    '治具から製品を取り出す
1505 '    '
1506 ''    Mov PProductOnJigGet_6          'ネジピックアップ向き合わせ位置
1507     M_25# = 3       '位置暫定
1508     Mov PProductOnJigGet_5          '製品チャック-ネジピックアップ向き合わせ中間位置
1509 '
1510     Mov PProductOnJigGet_4          '製品ピックアップ向き合わせ位置
1511     M_20# = MClear%
1512 *LOOP3
1513     If M_20# = MContinue% Then
1514         M_25# = 3
1515         M_20# = MClear%
1516     EndIf
1517     If M_26# <> 0 Then GoTo *LOOP3END Else GoTo *LOOP3
1518 *LOOP3END
1519     If M_26# = 1 Then GoTo *Set3End                            '正常終了ならエラー処理なし
1520     If M_In(11274) = 1 And M_In(11276) = 1 Then GoTo *Set3End  '今現在が正常終了と同じセンサー状態ならエラー処理なし
1521     If M_In(11276) = 0 Then                                    'センサーの状態を見てエラーを出す
1522         fErrorProcess(11,265,284,0)
1523     ElseIf M_In(11274) = 0 Then
1524         fErrorProcess(11,263,284,0)
1525     EndIf
1526     M_26# = 0
1527     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1528         Mov PProductOnJigGet_4
1529         Mov PProductOnJigGet_3
1530         Mov PProductOnPltSet_3
1531         Mov PProductOnPltSet_2
1532         Mov PInitialPosition
1533         Break
1534     EndIf
1535     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1536     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1537 '    If M_20# = MContinue% Then GoTo *RE_CCW_ROT
1538     If M_20# = MNext% Then M_20# = MClear%
1539     If M_20# = MContinue% Then GoTo *LOOP3
1540 '    *CompScrewSet5
1541 *Set3End
1542     M_26# = 0
1543 'ねじ締め順変更ここまで(6/9中村)
1544     '
1545     *RE_JIG_GET_1
1546     '
1547     M_Out(12259) = 0                '治具製品チャック閉OFF
1548     M_Out(12258) = 1                '治具製品チャック開ON
1549     M_Out(12261) = 0                '製品クランパー引込端OFF
1550     M_Out(12260) = 1                '製品クランパー出端ON
1551     M_Out(12256)= 0                 '製品チャック閉OFF
1552     M_Out(12257)= 1                 '製品チャック開ON
1553     '
1554 '    Wait M_In(11265)=1              '製品チャック開センサーON
1555     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '製品チャック開センサーON
1556     If MRtn = 1 Then GoTo *CompJigGet1
1557     fErrorProcess(11,244,284,0)
1558     If M_20# = MNext% Then M_20# = MClear%
1559     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1560         Mov PProductOnJigGet_3
1561         Mov PProductOnPltSet_3
1562         Mov PProductOnPltSet_2
1563         Mov PInitialPosition
1564         Break
1565     EndIf
1566     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1567     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1568     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1569     *CompJigGet1
1570     '
1571 '    Wait M_In(11272)=1              '治具製品クランパーセンサー開放端ON
1572     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '治具製品クランパーセンサー解放端ON
1573     If MRtn = 1 Then GoTo *CompJigGet2
1574     fErrorProcess(11,257,284,0)
1575     If M_20# = MNext% Then M_20# = MClear%
1576     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1577         Mov PProductOnJigGet_3
1578         Mov PProductOnPltSet_3
1579         Mov PProductOnPltSet_2
1580         Mov PInitialPosition
1581         Break
1582     EndIf
1583     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1584     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1585     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1586     *CompJigGet2
1587     '
1588     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)
1589     If MRtn = 1 Then GoTo *CompJigGet3
1590     fErrorProcess(11,259,284,0)
1591     If M_20# = MNext% Then M_20# = MClear%
1592     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1593         Mov PProductOnJigGet_3
1594         Mov PProductOnPltSet_3
1595         Mov PProductOnPltSet_2
1596         Mov PInitialPosition
1597         Break
1598     EndIf
1599     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1600     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1601     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1602     *CompJigGet3
1603     '
1604     Ovrd 100
1605     'Mov PProductOnJigGet_3          'ハンド手首回転(コメントアウト9/10中村)
1606     Mov PProductOnJigGet_2          '治具上空回避点
1607     Mvs PProductOnJigGet_1          '治具上空
1608     Ovrd 30
1609     Mvs PProductOnJigGet            '製品取り出し位置
1610     *RETRY_PRODUCT_ON_JIG_GET_2
1611     M_Out(12257)=0                  '製品チャック開OFF
1612     M_Out(12256)=1                  '製品チャック閉ON
1613 '    Wait M_In(11266)=1              '製品チャック閉センサーON
1614     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '製品チャック閉センサーON
1615     If MRtn = 1 Then GoTo *CompJigGet4
1616 '
1617     fErrorProcess(11,245,284,0)
1618     If M_20# = MNext% Then M_20# = MClear%
1619     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1620         M_Out(12256)=0                  '製品チャック閉OFF
1621         M_Out(12257)=1                  '製品チャック開ON
1622         Dly 2.0
1623         Mvs PProductOnJigGet_1
1624         Mvs PProductOnJigGet_2
1625         Mov PProductOnJigGet_3
1626         Mov PProductOnPltSet_3
1627         Mov PProductOnPltSet_2
1628         Mov PInitialPosition
1629         Break
1630     EndIf
1631     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1632     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1633     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1634     *CompJigGet4
1635     '
1636 '    Wait M_In(11264)=1              '製品検出センサーON
1637     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '製品検出センサーON
1638     If MRtn = 1 Then GoTo *CompJigGet5
1639     fErrorProcess(11,252,284,0)
1640     If M_20# = MNext% Then M_20# = MClear%
1641     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1642         M_Out(12256)=0                  '製品チャック閉OFF
1643         M_Out(12257)=1                  '製品チャック開ON
1644         Dly 2.0
1645         Mvs PProductOnJigGet_1
1646         Mvs PProductOnJigGet_2
1647         Mov PProductOnJigGet_3
1648         Mov PProductOnPltSet_3
1649         Mov PProductOnPltSet_2
1650         Mov PInitialPosition
1651         Break
1652     EndIf
1653     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1654     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1655     If M_20# = MContinue% Then GoTo *RE_JIG_GET_1
1656     *CompJigGet5
1657     '
1658     Dly 0.1
1659     Accel 50 , 100
1660     Mvs PProductOnJigGet_1          '治具上空
1661     Accel 100 , 100
1662     Ovrd 100
1663     Mvs PProductOnJigGet_2          '治具上空回避点
1664     Mov PProductOnJigGet_3          'パレット-治具中間点
1665     '
1666     '製品をパレットに置く
1667     '
1668     Ovrd 60
1669     Mov PProductOnPltSet_3          '通過点
1670     Mov PProductOnPltSet_2          'パレット回避点
1671     Ovrd 100
1672     Mov PProductOnPltSet_1          'パレット上空
1673     Ovrd 10
1674     Mvs PProductOnPltSet            'パレット置き位置
1675     Dly 0.2
1676     '
1677     *RE_PLT_SET_1
1678     '
1679     M_Out(12256)=0                  '製品チャック閉OFF
1680     M_Out(12257)=1                  '製品チャック開ON
1681     '
1682 '    Wait M_In(11265)=1              '製品チャック開センサーON
1683     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '製品チャック開センサーON
1684     If MRtn = 1 Then GoTo *CompPltSet1
1685     fErrorProcess(11,244,284,0)
1686     If M_20# = MNext% Then M_20# = MClear%
1687     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1688         Mvs PProductOnPltSet_1          'パレット上空
1689         Mov PProductOnPltSet_2          'パレット回避点
1690         Mov PInitialPosition
1691         Break
1692     EndIf
1693     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1694     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1695     If M_20# = MContinue% Then GoTo *RE_PLT_SET_1
1696     *CompPltSet1
1697     '
1698     Ovrd 100
1699     Mvs PProductOnPltSet_1          'パレット上空
1700     Mov PProductOnPltSet_2          'パレット回避点
1701     '
1702     *RE_PLT_SET_2
1703     '
1704 '    Wait M_In(11264) = 0            '製品検出センサーOFF
1705     MRtn = frInCheck(11264,0,MSETTIMEOUT05&)   '製品検出センサーOFF
1706     If MRtn = 1 Then GoTo *CompPltSet2
1707     fErrorProcess(11,253,284,0)
1708     If M_20# = MNext% Then M_20# = MClear%
1709     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '退避ルート
1710         Mov PInitialPosition
1711         Break
1712     EndIf
1713     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1714     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1715     If M_20# = MContinue% Then GoTo *RE_PLT_SET_2
1716     *CompPltSet2
1717     '
1718 '    Mov PInitialPosition            'プログラム原点
1719     MRtn = FnCtlValue2(2)       '組立ＯＫ＋１  2022/04/28 渡辺
1720     Mov PTicketRead_1
1721     MRtn = FnCtlValue2(99)      '読書開始信号OFF  2022/04/28 渡辺
1722     M_20# = MAssyOK%          '正常終了
1723 '
1724 *ASSY_ERROR_END
1725     M_Out(12268) = 0            '位置決め出OFF
1726     M_Out(12269) = 1            '位置決め戻ON
1727 *AssyEnd
1728 *fnAssyStart_FEndPosi
1729     Exit Function
1730 FEnd
1731 '
1732 '■fnPiasCheck
1733 ''' <summary>
1734 ''' PIASチケット読込み
1735 ''' </summary>
1736 ''' <returns>   0 : NG
1737 '''             1 : OK(読込み完了)
1738 ''' </returns>
1739 ''' <remarks>
1740 ''' Date   : 2021/07/07 : M.Hayakawa
1741 ''' </remarks>'
1742 Function M% fnPiasCheck
1743     fnPiasCheck = 0
1744     M_Out16(12576) = 79             'AUTO画面 PIASチケット読込み
1745     Wait M_In(MIN_IS_Ready%) = 1            'カメラ接続成功(M5370)
1746 '
1747 *RETRY_PIAS
1748     M_20# = MClear%
1749     M_Out16(12576) = 80             'AUTO画面 PIASチケット読込み
1750     '
1751     '【IDチケット読み込み】
1752     PInspPosition(1) = PTicketRead  'IDチケット読取位置
1753     MInspGroup%(1) = 1              '検査G番号
1754     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '画像処理検査実行
1755 '
1756     'エラーの場合
1757     If MRtn <> 1 Then
1758         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  'もう一度画像処理検査実行
1759         If MRtn <> 1 Then
1760             'D720 -> D1300 コピー要求
1761             M_Out(12565) = 1
1762             Dly 0.5
1763             M_Out(12565) = 0
1764             'エラー処理記述
1765             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1766             'GOT KEY入力待ち
1767             MKeyNumber = fnKEY_WAIT()
1768             '
1769             Select MKeyNumber
1770                 Case MNext%         '次へを選択した場合
1771                     M_20# = MPass%                          'M_20# プログラム間共通外部変数
1772                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1773                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1774                     Break
1775                 Case MAbout%        '停止を選択した場合
1776                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1777                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1778                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1779                     Break
1780                 Case MNgProcess%    'NGを選択した場合
1781                     M_20# = MAbout%                         'M_20# プログラム間共通外部変数
1782                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1783                     GoTo *fnPiasCheck_End                   'PIASチェック終了
1784                     Break
1785                 Case MContinue%     '継続を選択した場合
1786                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   'エラー画面消去
1787                     M_20# = MContinue%
1788                     GoTo *RETRY_PIAS                        'PIASチェックリトライ
1789                     Break
1790             End Select
1791         EndIf
1792     EndIf
1793 '----------D720 -> D1300 コピー要求----------
1794     M_Out(12565) = 1
1795     Dly 0.5
1796     M_Out(12565) = 0
1797 '----------通信確認をする----------
1798     fnAutoScreenComment(81) ' AUTO画面 PC通信確認
1799     MRtn = 0                ' 初期化
1800     M_20# = MClear%         ' 初期化
1801     MRtn = fnPCComuCheck()  ' PC-PLC通信チェック（M_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1802     ' 通信確認NG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1803     If MRtn <> 1 Then
1804         If M_20# = MContinue% Then
1805             GoTo *RETRY_PIAS         ' チケット読み直しからリトライ
1806         Else
1807             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1808         EndIf
1809     EndIf
1810 '----------工程抜け確認----------
1811     fnAutoScreenComment(82) ' AUTO画面 工程抜け確認
1812     MRtn = 0                ' 初期化
1813     M_20# = MClear%         ' 初期化
1814     MRtn = fnProcessCheck() ' 工程フラグチェック（M_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1815     ' 工程抜けNG時に外部変数の状態によりラベルジャンプ処理する（OK時はなにもしない）
1816     If MRtn <> 1 Then
1817         If M_20# = MContinue% Then
1818             GoTo *RETRY_PIAS         ' リトライはチケット読み直しから
1819         Else
1820             GoTo *fnPiasCheck_End    ' その他はPIASチェック終了
1821         EndIf
1822     EndIf
1823     '
1824     fnPiasCheck = 1
1825     *fnPiasCheck_End
1826     Exit Function
1827 FEnd
1828 '
1829 '■fnPCComuCheck
1830 ''' <summary>
1831 ''' PC-PLC通信チェック
1832 ''' </summary>
1833 ''' <returns>   0 : NG
1834 '''             1 : OK(読込み完了)
1835 ''' </returns>
1836 ''' <remarks>
1837 ''' Date   : 2021/07/07 : M.Hayakawa
1838 ''' </remarks>'
1839 Function M% fnPCComuCheck
1840     fnPCComuCheck = 0
1841     MJudge% = 0                                  '初期化
1842     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC通信確認要求(M300)
1843     Wait M_In(11575) = 1                         'M5575  toRBT_通信確認統合返信
1844     '
1845     For MStaNo = 0 To 5
1846         '
1847         If M_In(MIN_PIAS_ComOK%) = 1 Then
1848             'PC通信OK(M400)
1849             MJudge% = MOK%
1850             MStaNo = 5
1851             Break
1852         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1853             'toRBT_通信確認time out
1854             MJudge% = MNG%
1855             MCommentD1001 = 15
1856             MCommentD1002 = 21
1857             MStaNo = 5
1858             Break
1859         Else
1860             'toRBT_通信確認time out
1861             MJudge% = MNG%
1862             MCommentD1001 = 14
1863             MCommentD1002 = 21
1864             Break
1865         EndIf
1866     Next MStaNo
1867     '
1868     '上記で返信フラグを受信してからPC通信確認OFF
1869     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC内でM300を保持しているのでRBTでは解除
1870     '
1871     'エラー画面
1872     If MJudge% <> MOK% Then
1873         M_20# = MClear%     '初期化
1874         'エラー処理記述
1875         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1876         'GOT KEY入力待ち
1877         MKeyNumber = fnKEY_WAIT()
1878         '
1879         If MKeyNumber = MAbout% Then            '停止を選択した場合
1880             M_20# = MAbout%                     'M_20# プログラム間共通外部変数
1881             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1882             Break
1883         ElseIf MKeyNumber = MNext% Then         '次へを選択した場合
1884             M_20# = MNext%                      'M_20# プログラム間共通外部変数
1885             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1886             Break
1887         ElseIf MKeyNumber = MContinue% Then     '停止を選択した場合
1888             M_20# = MContinue%                  'M_20# プログラム間共通外部変数
1889             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1890             Break
1891         ElseIf MKeyNumber = MNgProcess% Then    '次へを選択した場合
1892             M_20# = MNgProcess%                 'M_20# プログラム間共通外部変数
1893             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
1894             Break
1895         EndIf
1896     Else
1897         'OKの場合
1898         fnPCComuCheck = 1
1899     EndIf
1900     Exit Function
1901 FEnd
1902 '
1903 '■fnProcessCheck
1904 ''' <summary>
1905 ''' 工程抜け確認
1906 ''' </summary>
1907 ''' <returns>    1：工程履歴OK     0：異常終了
1908 '''             -1：前工程履歴NG  -2：自工程履歴あり
1909 '''             -3：モデル仕向NG  -4：タイムアウト
1910 '''             -5：履歴処理エラー
1911 ''' </returns>
1912 ''' <remarks>
1913 ''' Date   : 2021/07/07 : M.Hayakawa
1914 ''' </remarks>'
1915 Function M% fnProcessCheck
1916     fnProcessCheck = 0
1917     MJudge% = MNG%      '一旦NGを初期化とする
1918 '----------工程抜け確認----------
1919     MCommentD1001 = 0   'コメント初期化
1920     For MStaNo = 0 To 5
1921         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC工程抜け確認要求(M302)
1922         Wait M_In(11577) = 1                            'M5577  toRBT_PC工程抜け確認統合返信
1923         '
1924         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 履歴OK M407
1925             MJudge% = MOK%
1926             fnAutoScreenComment(85)     ' AUTO画面
1927             MStaNo = 5
1928             Break
1929         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 自工程履歴あり M426
1930             MFlgLoop% = 0
1931             MJudge% = MNG%
1932             MCommentD1001 = 27
1933             MCommentD1002 = 22
1934             fnAutoScreenComment(94)     ' AUTO画面
1935             fnProcessCheck = -2         ' NGは-2を返す
1936             MStaNo = 5
1937             Break
1938         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 モデル仕向NG M406
1939            MJudge% = MNG%
1940             MCommentD1001 = 31
1941             MCommentD1002 = 22
1942             fnAutoScreenComment(83)     ' AUTO画面
1943             fnProcessCheck = -3         ' NGは-3を返す
1944             MStaNo = 5
1945             Break
1946         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 前工程履歴NG M408
1947             '履歴NGは直ぐに終了せず繰り返し確認を行う
1948             '前工程の書込みが終了していない可能性があるため
1949             MJudge% = MNG%
1950             MCommentD1001 = 32
1951             MCommentD1002 = 22
1952             fnAutoScreenComment(84)     ' AUTO画面
1953             fnProcessCheck = -1         ' NGは-1を返す
1954             Dly 1.0
1955             '工程抜け確認OFF
1956             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC工程抜け確認要求(M302)
1957             Dly 1.0
1958            'MStaNo = 5
1959             Break
1960         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 履歴処理エラー M432
1961             MFlgLoop% = 0
1962             MJudge% = MNG%
1963             MCommentD1001 = 29
1964             MCommentD1002 = 22
1965             fnAutoScreenComment(86)     ' AUTO画面 履歴処理エラー
1966             fnProcessCheck = -5         ' NGは-5を返す
1967             MStaNo = 5
1968             Break
1969         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    'タイムアウト
1970             MJudge% = MNG%
1971             If MCommentD1001 = 32 Then
1972                 '何もしない
1973             Else
1974                 MCommentD1001 = 26
1975             EndIf
1976             MCommentD1002 = 22
1977             fnProcessCheck = -4         ' NGは-4を返す
1978             MStaNo = 5
1979             Break
1980         Else
1981             MJudge% = MNG%
1982             MCommentD1001 = 28
1983             MCommentD1002 = 22
1984         EndIf
1985     Next MStaNo
1986     '工程抜け確認OFF
1987     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC工程抜け確認要求(M302)
1988     '通過履歴NG 工程抜けの場合
1989     If MJudge% = MPass% Then
1990         M_20# = MPass%
1991     EndIf
1992     '
1993     'エラー画面
1994     If MJudge% <> MOK% Then
1995         M_20# = MClear%     '初期化
1996         'エラー処理記述
1997         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1998         'GOT KEY入力待ち
1999         MKeyNumber = fnKEY_WAIT()
2000         '
2001         Select MKeyNumber
2002             Case MAbout%        '停止を選択した場合
2003                 M_20# = MAbout%         'M_20# プログラム間共通外部変数
2004                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2005                 Break
2006             Case MNext%         '次へを選択した場合
2007                 M_20# = MPass%          'M_20# プログラム間共通外部変数
2008                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2009                 Break
2010             Case MContinue%     '継続を選択した場合
2011                 M_20# = MContinue%      'M_20# プログラム間共通外部変数
2012                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2013                 Break
2014             Case MNgProcess%    'NGを選択した場合
2015                 M_20# = MNgProcess%     'M_20# プログラム間共通外部変数
2016                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2017                 Break
2018         End Select
2019     Else
2020         fnProcessCheck = 1  ' OKは1を返す
2021     EndIf
2022     Exit Function
2023 FEnd
2024 '
2025 '■fnPiasWrite
2026 ''' <summary>
2027 ''' Pias 組立結果書込み要求
2028 ''' </summary>
2029 '''<param name="MFlg%">
2030 '''                 MOK%(1) = 工程履歴にOKを書込む
2031 '''                 MNG%(0) = 工程履歴にNGを書込む
2032 '''</param>
2033 '''<returns></returns>
2034 ''' <remarks>
2035 ''' Date   : 2021/07/07 : M.Hayakawa
2036 ''' </remarks>'
2037 Function M% fnPiasWrite(ByVal MFlg%)
2038       fnPiasWrite = 0
2039 *RETRY_PIASWRITE
2040     '
2041     '組立OK(MOK%)の場合　M306 ON
2042    '組立NG(MNG%)の場合　M307 ON
2043     If MFlg% = MOK% Then
2044         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
2045     Else
2046         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
2047     EndIf
2048     Dly 0.1                  '念のため
2049     '
2050     'Piasへ書込み開始 M305 -> ON
2051     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
2052     Wait M_In(11582) = 1                        '組立完了統合返信 M5582
2053     '
2054     MJudge% = MNG%
2055     '
2056     For MStaNo = 0 To 5
2057         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 工程履歴処理OK
2058             MJudge% = MOK%
2059             'MRet = fnAutoScreenComment(85)  'AUTO画面
2060             MStaNo = 5
2061             Break
2062         '
2063         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 工程履歴処理NG
2064             MJudge% = MNG%
2065             'MRet = fnAutoScreenComment(85)  'AUTO画面
2066            MCommentD1001 = 34
2067            MCommentD1002 = 25
2068             MStaNo = 5
2069             Break
2070         '
2071         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 工程履歴処理エラー(なんかのトラブル)
2072             MJudge% = MNG%
2073             'MRet = fnAutoScreenComment(85)  'AUTO画面
2074            MCommentD1001 = 35
2075            MCommentD1002 = 25
2076             MStaNo = 5
2077             Break
2078         '
2079         ElseIf M_In(11583) = 1 Then                         '工程履歴処理time out
2080             MJudge% = MNG%
2081             'MRet = fnAutoScreenComment(85)  'AUTO画面
2082            MCommentD1001 = 36
2083            MCommentD1002 = 25
2084             MStaNo = 5
2085             Break
2086         '
2087         Else
2088             MJudge% = MNG%
2089            MCommentD1001 = 42
2090            MCommentD1002 = 25
2091         '
2092         EndIf
2093         '
2094     Next MStaNo
2095     '
2096     'Piasへ書込み開始 M305 -> OfF
2097     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
2098     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
2099     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
2100     '
2101     '
2102     '通過履歴NG 工程抜けの場合
2103     If MJudge% = MPass% Then
2104         M_20# = MPass%
2105     EndIf
2106     '
2107    M_20# = MClear%     '初期化
2108     '
2109     'エラー画面
2110     If MJudge% < MOK% Then
2111     '
2112 '残しておくが現状では使用しないラベル
2113 *RETRY_ERR_WRITE
2114         M_20# = MClear%     '初期化
2115         'エラー処理記述
2116         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
2117         'GOT KEY入力待ち
2118         MKeyNumber = fnKEY_WAIT()
2119         '
2120         If MKeyNumber = MAbout% Then   '停止を選択した場合
2121             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2122            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2123             Break
2124         '
2125         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2126             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2127             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2128         '
2129         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2130             M_20# = MPass%            'M_20# プログラム間共通外部変数
2131             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2132         '
2133         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2134             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2135            fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2136             Break
2137         '
2138         EndIf
2139         '
2140         If M_20# = MClear% Then *RETRY_ERR_WRITE
2141         '
2142     EndIf
2143     '
2144     If M_20# = MContinue% Then *RETRY_PIASWRITE
2145     '
2146     fnPiasWrite = 1
2147     Exit Function
2148 FEnd
2149 '
2150 '■fnPCBNumberCheck
2151 ''' <summary>
2152 ''' Pias 基板番号照合要求
2153 ''' </summary>
2154 '''<param name="%"></param>
2155 '''<param name="%"></param>
2156 '''<returns></returns>
2157 ''' <remarks>
2158 ''' Date   : 2021/07/07 : M.Hayakawa
2159 ''' </remarks>'
2160 Function M% fnPCBNumberCheck
2161       fnPCBNumberCheck = 0
2162     '
2163 *RETRY_PCBCHECK
2164     fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
2165     'Piasへ基板照合開始 M310 -> ON
2166     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
2167     Wait M_In(11579) = 1                        '基板番号統合返信 M5579
2168     '
2169     MJudge% = MNG%
2170     '
2171     For MStaNo = 0 To 5
2172         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 基板番号処理OK
2173             MJudge% = MOK%
2174             fnAutoScreenComment(96)  'AUTO画面
2175             MStaNo = 5
2176             Break
2177         '
2178         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 基板番号NG
2179             MJudge% = MNG%
2180             fnAutoScreenComment(97)  'AUTO画面
2181             MCommentD1001 = 37
2182             MCommentD1002 = 25
2183             MStaNo = 5
2184             Break
2185         '
2186         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 基板番号処理エラー(なんかのトラブル)
2187             MJudge% = MNG%
2188             fnAutoScreenComment(98)  'AUTO画面
2189             MCommentD1001 = 38
2190             MCommentD1002 = 25
2191             MStaNo = 5
2192             Break
2193         '
2194         ElseIf M_In(11580) = 1 Then                         'time out
2195             MJudge% = MNG%
2196             fnAutoScreenComment(99)  'AUTO画面
2197             MCommentD1001 = 39
2198             MCommentD1002 = 25
2199             MStaNo = 5
2200             Break
2201         '
2202         Else
2203             MJudge% = MNG%
2204            MCommentD1001 = 41
2205            MCommentD1002 = 25
2206         '
2207         EndIf
2208         '
2209     Next MStaNo
2210     '
2211     'Piasへ基板照合開始 M310 -> OfF
2212     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
2213     '
2214     '
2215     '通過履歴NG 工程抜けの場合
2216     If MJudge% = MPass% Then
2217         M_20# = MPass%
2218     EndIf
2219     '
2220    M_20# = MClear%     '初期化
2221     '
2222     'エラー画面
2223     If MJudge% < MOK% Then
2224     '
2225 '残しておくが現状では使用しないラベル
2226 *RETRY_ERR_PCBNUMBER
2227         M_20# = MClear%     '初期化
2228         'エラー処理記述
2229         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
2230         'GOT KEY入力待ち
2231         MKeyNumber = fnKEY_WAIT()
2232         '
2233         If MKeyNumber = MAbout% Then   '停止を選択した場合
2234             M_20# = MAbout%            'M_20# プログラム間共通外部変数
2235             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2236             Break
2237         '
2238         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
2239             M_20# = MContinue%            'M_20# プログラム間共通外部変数
2240             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2241         '
2242         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
2243             M_20# = MPass%            'M_20# プログラム間共通外部変数
2244             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2245         '
2246         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
2247             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
2248             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
2249             Break
2250         '
2251         EndIf
2252         '
2253         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
2254         '
2255     EndIf
2256     '
2257     If M_20# = MContinue% Then *RETRY_PCBCHECK
2258     Exit Function
2259 FEnd
2260 '
2261 '■ScrewTight
2262 ''' <summary>
2263 ''' ねじ締めを行う(Sタイト)
2264 ''' </summary>
2265 '''<param name="PScrewPos()">
2266 '''             PScrewPos(1)    ：パレット上ねじ締めS①の安全回避位置  +30
2267 '''             PScrewPos(2)    ：ねじ締め回避点
2268 '''             PScrewPos(10)   ：ねじ締め終了高さ
2269 '''<param name="MScrewType">ネジタイプ(mm/sec)
2270 '''             1:6mm Sタイト銀ネジ
2271 '''             2:13mm Sタイト銀ネジ
2272 '''             3:6mm Sタイト黒ネジ
2273 '''             4:3mm Sタイト黒ネジ
2274 '''             5:6mm Mネジ
2275 '''             6:13mm Sタイト銀ネジ
2276 '''</param>
2277 '''<param name="MFeedSpd">送り速度(mm/sec)</param>
2278 '''<returns>整数
2279 '''         0=異常終了、1=正常終了
2280 '''</returns>
2281 ''' <remarks>
2282 ''' Date   : 2021/07/07 : M.Hayakawa
2283 ''' Update : 2021/09/28 : M.Hayakawa ネジタイプ、送り速度を引数に追加
2284 ''' </remarks>'
2285 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   'ネジ締め個別設定
2286     fnAutoScreenComment(521)    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2287     ScrewTight = 0
2288     MOKNGFlg = 0
2289     Ovrd 100
2290     Fine 0.05 , P
2291     Mvs PScrewPosition(1)       ' パレット上ねじ締めS①の安全回避位置
2292     Select MScrewType%      '読み込み位置変更(1/19中村)
2293         Case 1
2294             ' Sタイト：プログラム1、バンク1に設定
2295             ProgramBankSet(1,1)
2296             Break
2297         Case 2
2298             ' Sタイト13mm：プログラム2、バンク1に設定
2299             ProgramBankSet(2,1)
2300             Break
2301         Case 3
2302             ' Sタイト黒：プログラム3、バンク1に設定
2303             ProgramBankSet(3,1)
2304             Break
2305         Case 4
2306             ' Sタイト3mm黒：プログラム4、バンク1に設定
2307             ProgramBankSet(4,1)
2308             Break
2309         Case 5
2310             ' Mネジ：プログラム5、バンク1に設定
2311             ProgramBankSet(5,1)
2312             Break
2313         Case 6
2314             ' Sタイト13mm(パラメータ違い):プログラム2、バンク2に設定
2315             ProgramBankSet(2,2)
2316             Break
2317         Default
2318             ' プログラム1、バンクなし設定
2319             ProgramBankSet(0,0)
2320             Break
2321     End Select
2322     Accel 100,10
2323 '    Ovrd MOvrdA%               '10/7現在値Null
2324     Ovrd 60                     '念のため減速 数値変更 林
2325     ' パレット上ねじ締め開始位置へ移動
2326     Mvs PScrewPosition(2)
2327     ' 内部Ovrd設定
2328 '    Ovrd MOvrdA%
2329     Ovrd 100
2330     Accel
2331     ' Spd設定
2332     Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
2333 '    Spd MFeedSpd
2334     ' 設定進み量5.0 × 操作パネルのオーバーライド係数 × プログラム内オーバーライド係数
2335     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
2336 '    Select MScrewType%      '読み込み位置変更(1/19中村)
2337 '        Case 1
2338 '            ' Sタイト：プログラム1、バンク1に設定
2339 '            ProgramBankSet(1,1)
2340 '            Break
2341 '        Case 2
2342 '            ' Sタイト13mm：プログラム2、バンク1に設定
2343 '            ProgramBankSet(2,1)
2344 '            Break
2345 '        Case 3
2346 '            ' Sタイト黒：プログラム3、バンク1に設定
2347 '            ProgramBankSet(3,1)
2348 '            Break
2349 '        Case 4
2350 '            ' Sタイト3mm黒：プログラム4、バンク1に設定
2351 '            ProgramBankSet(4,1)
2352 '            Break
2353 '        Case 5
2354 '            ' Mネジ：プログラム5、バンク1に設定
2355 '            ProgramBankSet(5,1)
2356 '            Break
2357 '        Default
2358 '            ' プログラム1、バンクなし設定
2359 '            ProgramBankSet(0,0)
2360 '            Break
2361 '    End Select
2362 '
2363 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     'ドライバーON　CW
2364      'ドライバーON　CW
2365     M_Out(12241)=1
2366     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
2367     Wait M_In(11584)=1          '完了/エラー検出 暫定コメント 10/6 M.H
2368     Dly 0.1
2369     Spd M_NSpd
2370     Fine 0 , P
2371     '
2372     If M_In(11256)=1 Then  'ねじトータルエラー検出時
2373         M_Out(Y61_Driver)=0     'ドライバーOFF　CW
2374         Dly 0.1
2375        ' プログラム・バンク解除
2376         ProgramBankSet(0,0)
2377         'パレット上ねじ締め終了位置上空へ移動
2378         Mvs PScrewPosition(10),-80
2379         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
2380         M_Out(12249)=1 Dly 0.3
2381         MOKNGFlg = -1
2382         ScrewTight = 0
2383     Else
2384          'ドライバーOFF　CW
2385         M_Out(12241)=0
2386 ''        エラーがない場合はネジ締め終了位置で増し締め
2387 '        Select MScrewType%
2388 '            Case 1
2389 '                ' Sタイト：プログラム1、バンク3に設定
2390 '                ProgramBankSet(1,3)
2391 '                Break
2392 '            Case 2
2393 '                ' Sタイト13mm：プログラム2、バンク3に設定
2394 '                ProgramBankSet(2,3)
2395 '                Break
2396 '            Case 3
2397 '                ' Sタイト黒：プログラム1、バンク3に設定
2398 '                ProgramBankSet(3,3)
2399 '                Break
2400 '            Case 4
2401 '                ' Sタイト13mm：プログラム1、バンク3に設定
2402 '                ProgramBankSet(4,3)
2403 '                Break
2404 '            Case 5
2405 '                ' Mネジ：プログラム1、バンク3に設定
2406 '                ProgramBankSet(5,3)
2407 '                Break
2408 '            Default
2409 '                ' プログラム1、バンクなし設定
2410 '                ProgramBankSet(0,0)
2411 '                Break
2412 '        End Select
2413 '         'ドライバーON　CW
2414 '        Mvs PScrewPosition(10)
2415 '        M_Out(12241)=1
2416 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   'ねじ締め終了高さまで移動中エラー検出
2417 '
2418          'ドライバーOFF　CW
2419         M_Out(12241)=0
2420        ' プログラム・バンク解除
2421         ProgramBankSet(0,0)
2422         'ねじ吸着　OFF'M_Out(Y68_VV1)=0からM_Out(12249)=1 Dly 0.3へ変更(8/27中村)
2423         M_Out(12249)=1 Dly 0.3
2424     '     ↓PScrewPos(2) → PScrewPosition(10)に変更 9/16 M.Hayakawa
2425         'パレット上ねじ締め終了位置上空へ移動
2426         Mvs PScrewPosition(10),-80
2427         ScrewTight = 1
2428     EndIf
2429 ' 暫定（暫定マスク　9/16 M.Hayakawa)
2430 '    Ovrd 10
2431 '    Mvs PScrewPosition(1)        ' パレット上ねじ締めS①の安全回避位置
2432     Ovrd 100
2433     Exit Function
2434 FEnd
2435 '
2436 '■ScrewGet
2437 ''' <summary>
2438 ''' ねじ供給機からねじを得る
2439 ''' </summary>
2440 '''<param name="%">
2441 '''         PScrewPos(1)    ：ねじ供給器のねじ上空
2442 '''         PScrewPos(2)    ：ねじ供給器回避点
2443 '''         PScrewPos(9)    ：ねじ供給器上空ネジ捨位置
2444 '''         PScrewPos(10)   ：ねじ供給器のねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2445 '''         PScrewPos(3)    ：Mねじポカヨケ位置
2446 '''         PScrewPos(4)    ：Mねじポカヨケ位置　上空
2447 '''</param>
2448 '''<param name = FeederReadyNo%> 指定の供給機Ready </param>
2449 '''<param name = FeederScrewSensor%> 指定の誤供給防止センサー指定(0でセンサー無し)</param>
2450 '''<returns>整数
2451 '''         0=異常終了、1=正常終了、-1=ねじ供給NG、-2=ねじ誤供給NG、-3=吸着エラー
2452 '''</returns>
2453 ''' <remarks>
2454 ''' Date   : 2021/07/07 : M.Hayakawa
2455 ''' </remarks>
2456 '''<update>
2457 '''Date    : 2021/11/15 : 中村
2458 '''</update>
2459 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
2460     fnAutoScreenComment(522)    '状態表示[ネジ供給待ち] 2022/05/09 渡辺
2461     ScrewGet = 0
2462     MScrewJudge% = 0
2463     'ねじ供給器初期動作エラーチェック
2464     Mov PScrewPosition(2)   'ねじ供給機回避点へ移動
2465     For MCnt% = 0 To MFinCnt%
2466         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '指定ねじ供給機がReadyになっているか確認(5秒間)
2467         If MRtn = 0 Then
2468             M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
2469             ScrewGet = -1
2470             MScrewJudge% = 2
2471         EndIf
2472         Ovrd 100
2473         If FeederScrewSensor% <> 0 Then
2474             If M_In(FeederScrewSensor%) = 1 Then  '誤供給が検出されたら
2475                 'Ovrd 30
2476                 M_Out(12249)=1 Dly 0.3     'ねじ吸着 Off(念のため)
2477                 'NGとしてここの関数から抜ける
2478                 ScrewGet = -2
2479                 MScrewJudge% = 3
2480             EndIf
2481         EndIf
2482         Ovrd 100
2483         Spd M_NSpd
2484         If MScrewJudge% = 0 Then
2485     '        ScrewGet = 0
2486             M_Out(Y63_Driver)=1         ' バンクセッティング　C2
2487             Dly 0.3
2488             MScrewCnt% = 0
2489             MFinCnt% = 2
2490             fnAutoScreenComment(521)     '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2491             Mov PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
2492             'Ovrd 40 '2に変更 10/6 M.H '5に変更10/7中村
2493             'ねじっこ(Sネジ）ねじピックアップ位置：ねじ頭とビットとのクリアランスは0.3mmが理想
2494             'ネジとビット篏合させる 吸着位置から1.2下げて篏合
2495             'Mvs PScrewPosition(10), 1.2
2496            Mvs PScrewPosition(10)       'Fan用ねじ吸着位置修正のため変更 2022-02-01AJI
2497             'ビット回転(処理位置変更)
2498             M_Out(Y60_Driver)=1
2499             M_Timer(4) = 0
2500             MloopFlg = 0
2501             MCntTime& = 0
2502             While MloopFlg = 0
2503                 MCrtTime& = M_Timer(4)
2504                 If MCrtTime& >= 180 Then
2505                     MloopFlg = 1
2506                 EndIf
2507             WEnd
2508             M_Out(Y68_VV1)=1 Dly 0.3     ' ねじ吸着　ON'Dly 0.3追加(8/27中村)
2509             '吸着確認
2510             MRtn = 0
2511             MRtn = frScrewInCheck(11267, 1, MSETTIMEOUT01&)
2512 '            'ビット回転(処理位置変更)
2513 '            M_Out(Y60_Driver)=1
2514 '            Dly 0.2
2515             '
2516             JOvrd M_NJovrd
2517             Spd M_NSpd
2518             Ovrd 50
2519             'ネジ吸着確認位置移動
2520             Mvs PScrewPosition(10)       ' 念のため一旦、旧ねじ吸着位置
2521             Mvs PScrewPosition(1)        ' ネジ吸着確認位置(10),-30から(1)へ変更7/25中村
2522             Ovrd 100
2523            'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
2524             'ビット回転停止
2525             M_Out(Y60_Driver)=0
2526             '
2527 '            If MRtn = 1 Then           '最初の閾値を確認しない
2528                 '1秒間ネジ吸着確認
2529                 MRtn = frScrewInCheck(11268, 1, MSETTIMEOUT01&)
2530 '            MRtn = 1 '強制OK
2531 '            EndIf
2532             'MRtn = 0'強制エラー
2533             '吸着エラーの場合
2534             'ネジをねじ太郎に戻す
2535             If MRtn = 0 Then
2536                 Ovrd 30      '2から5に変更
2537                 'ビット回転停止
2538                 M_Out(Y60_Driver)=0
2539                 'ネジ供給機上空
2540                 Mvs PScrewPosition(1)
2541                 '更に上空
2542                 Mov PScrewPosition(1), -140
2543                 'ネジ捨て位置
2544                 If FeederReadyNo% = 11259 Then     '供給機別に吸着エラー数をカウント　2022/05/19 渡辺
2545                     MRtn = FnCtlValue2(3)          '供給機２吸着エラー数＋１
2546                 Else
2547                     MRtn = FnCtlValue2(4)          '供給機１吸着エラー数＋１  2022/04/28 渡辺
2548                 EndIf
2549                 Mov PScrewPosition(9)
2550                 MRtn = FnCtlValue2(99)         '読書開始信号OFF  2022/04/28 渡辺
2551                 '吸着OFF
2552                 M_Out(12249)=1                  'ねじ吸着　OFF(パルス出力取りやめ22/9/13中村)
2553                 Dly 0.2
2554                 '破壊ON
2555                 M_Out(Y6B_VB1)=1 '真空破壊ON
2556                 'ビット回転
2557                 M_Out(Y61_Driver)=1
2558                 Dly 0.5
2559                 '                '
2560                 Ovrd 100
2561                 JOvrd M_NJovrd
2562                 Spd M_NSpd
2563                 'ドライバーを上下させねじを振り落とす
2564                 Mov PScrewPosition(9), 10
2565                 Mov PScrewPosition(9)
2566                 Dly 0.1
2567                 Mov PScrewPosition(9), 10
2568                 Mov PScrewPosition(9)
2569                 '
2570                 'ネジ落ち待ち
2571                 Wait M_In(11272) = 0
2572                 'ビット回転停止
2573                 M_Out(Y61_Driver)=0
2574                 Dly 0.1
2575                 '破壊OFF
2576                 M_Out(Y6B_VB1)=0 '真空破壊OFF
2577                 M_Out(12249)=0                  'ねじ吸着　OFF
2578                 'ねじ落ちたとして、移動更に上空
2579                 Mov PScrewPosition(1), -140
2580                 Ovrd 100
2581                 Spd M_NSpd
2582                 'ネジ供給機上空
2583                 Mvs PScrewPosition(1)
2584 '                '
2585                 ScrewGet = -3
2586                 If MCnt% = MFinCnt% Then
2587                     MScrewJudge% = 4
2588                     Mov PScrewPosition(2)
2589                     Break
2590                 EndIf
2591                 Break
2592 '                '
2593             Else
2594                 MCnt% = MFinCnt%
2595                 ScrewGet = 1
2596             EndIf
2597         Else
2598             MCnt% =MFinCnt%
2599         EndIf
2600     Next  MCnt%
2601         '
2602 '    If MScrewJudge% = 0 Then
2603 '        Ovrd 100
2604 '        Spd M_NSpd
2605 '        PScrewPosition(1)
2606 '        Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
2607 '        'Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
2608 '        M_Out(Y60_Driver)=0     ' ビット回転停止
2609 '        M_Out(Y63_Driver)=0     ' バンクセッティング　C2
2610 '        'Mvs PScrewPosition(10), -30  ' ねじピックアップ位置 -30mm
2611 '        Mvs PScrewPosition(1)        ' ねじ供給機(Sネジ）上空
2612 '        'Mov PScrewPosition(2)
2613 '        'もう一度吸着確認　上空の最終閾値
2614 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
2615 '        If MRtn = 0 Then      '吸着エラーの場合
2616 '            MScrewJudge% = 4
2617 '            ScrewGet = -3
2618 '        ElseIf MRtn = 1 Then      '吸着OKの場合
2619 '            MScrewJudge% = 1
2620 '            ScrewGet = 1
2621 '        EndIf
2622 '        Break
2623 '    EndIf
2624     '
2625     Mov PScrewPosition(2)
2626 '
2627 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
2628     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '正常終了時ラベルにジャンプ
2629     '
2630     Select MScrewJudge%
2631 '        Case 0
2632 ''            fErrorProcess(11,162,163,0) '異常終了
2633 '            MCommentD1001 = 162
2634 '            MCommentD1002 = 96
2635 '            Break
2636         Case 2
2637 '            fErrorProcess(11,63,161,0) '供給NG
2638             MCommentD1001 = 63
2639             MCommentD1002 = 96
2640             Break
2641         Case 3
2642 '            fErrorProcess(11,160,164,0) '誤供給
2643             MCommentD1001 = 237
2644             MCommentD1002 = 96
2645             Break
2646         Case 4
2647 '            fErrorProcess(11,94,95,0) '吸着NG
2648             MCommentD1001 = 94
2649             MCommentD1002 = 95
2650             Break
2651     End Select
2652     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
2653     '
2654     Select M_20#
2655         Case MAbout%          '停止が押された場合
2656             Mov PScrewPosition(2)                  '回避点に移動して関数を抜ける
2657             Break
2658         Case MContinue%       'リトライが押されていた場合(関数を抜けた先で処理)
2659             Break
2660         Case MNext%           '継続が押された場合
2661             M_20# = MClear%     '初期化
2662             Break
2663         Case MNgProcess%      'NGが押された場合
2664             Mov PScrewPosition(2)   '回避点に移動して関数を抜ける
2665             Break
2666         End Select
2667 *End_ScrewGet
2668     Exit Function
2669 FEnd
2670 '
2671 '■ProgramBankSet
2672 ''' <summary>
2673 ''' ねじ締めを行う(Pタイト)
2674 ''' </summary>
2675 '''<param name="MProgramNo">プログラム番号</param>
2676 '''<param name="MBankNo">バンク番号</param>
2677 '''</returns>
2678 ''' <remarks>
2679 ''' Date   : 2021/10/05 : M.Hayakawa
2680 ''' </remarks>'
2681 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
2682 '
2683     MLocalPrgNo% = (MProgramNo% - 1) * 32
2684     MLocalBankNo% = MBankNo% * 4
2685 '
2686     If MLocalPrgNo% >= 0 And MLocalBankNo% >= 0 Then
2687         MLocalOutNo% = MLocalPrgNo% + MLocalBankNo%
2688     Else
2689         MLocalOutNo% = 0
2690     EndIf
2691 '
2692     M_Out8(12240) = MLocalOutNo%
2693     Dly 0.1
2694     Exit Function
2695 FEnd
2696 '
2697 '■fnKEY_WAIT()
2698 ''' <summary>
2699 ''' GOTからのキー入力待ち
2700 ''' </summary>
2701 '''<returns>1：停止    2：次へ
2702 '''         3：継続    4：トルクチェック開始
2703 '''         5：NG
2704 '''         11：ロボット初期位置1    12：ロボット初期位置2
2705 '''         13：ロボット初期位置3    14：ロボット初期位置4
2706 '''</returns>
2707 ''' <remarks>
2708 ''' Date   : 2021/07/07 : M.Hayakawa
2709 ''' </remarks>'
2710 Function M% fnKEY_WAIT()
2711     fnKEY_WAIT = 0
2712     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT 青点灯
2713     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT 赤点滅
2714     MRtn = fnAUTO_CTL()                        'AUTOモード停止、継続キー入力待ち
2715     '下記キー待ちの継続に反応させないため
2716     Wait M_In(11347) = 0                'toRBT_継続の完了待ち
2717     Dly 0.2
2718     Wait M_In(11347) = 0                'toRBT_継続の完了待ち　2重確認
2719     MLocalLoopFlg=1
2720     While MLocalLoopFlg=1
2721         If M_In(11345) = 1 Then         '停止   M5345
2722             M_Out(12343) = 1 Dly 0.5    '停止要求受信パルス M6343
2723             fnKEY_WAIT = 1
2724             MLocalLoopFlg=-1
2725             Break
2726         ElseIf M_In(11346) = 1 Then     'fromPLC_次へ   M5346
2727             M_Out(12348) = 1 Dly 1.0    '次へ要求受信パルス M6348
2728             fnKEY_WAIT = 2
2729             MLocalLoopFlg=-1
2730             Break
2731         ElseIf M_In(11356) = 1 Then     'fromPLC_継続2  M5356
2732             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT継続2要求受信 M6344
2733             fnKEY_WAIT = 3
2734             MLocalLoopFlg=-1
2735             Break
2736         ElseIf M_In(11355) = 1 Then     'fromPLC_トルクチェック開始要求
2737             M_Out(12342) = 1 Dly 0.5    'toPLC_RBTトルクチェック開始要求受信パルス M6342
2738             fnKEY_WAIT = 4
2739             MLocalLoopFlg=-1
2740             Break
2741         ElseIf M_In(11357) = 1 Then     'fromPLC_NG要求
2742             M_Out(12349) = 1 Dly 1.0    'toPLC_NG受信パルス M6349
2743             fnKEY_WAIT = 5
2744             MLocalLoopFlg=-1
2745             Break
2746             '
2747         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_ロボット初期位置1要求 M5568
2748             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置1受信 M6560
2749             fnKEY_WAIT = MRobotInit1%
2750             MLocalLoopFlg=-1
2751             Break
2752             '
2753         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_ロボット初期位置2要求 M5569
2754             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_ロボット初期位置2受信 M6561
2755             fnKEY_WAIT = MRobotInit2%
2756             MLocalLoopFlg=-1
2757             Break
2758             '
2759         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_ロボット初期位置3要求 M5570
2760             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置3受信 M6562
2761             fnKEY_WAIT = MRobotInit3%
2762             MLocalLoopFlg=-1
2763             Break
2764             '
2765         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_ロボット初期位置4要求 M5571
2766             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_ロボット初期位置4受信 M6563
2767             fnKEY_WAIT = MRobotInit4%
2768             MLocalLoopFlg=-1
2769             Break
2770             '
2771         Else
2772         EndIf
2773     WEnd
2774     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT 青点灯
2775     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT 赤点滅
2776     Exit Function
2777 FEnd
2778 '
2779 '■ fnAUTO_CTL
2780 ''' <summary>
2781 ''' AUTOモードOFF、PLCからの開始待ち
2782 ''' </summary>
2783 ''' <remarks>
2784 ''' Date   : 2021/07/07 : M.Hayakawa
2785 ''' </remarks>
2786 Function M% fnAUTO_CTL
2787     fnAUTO_CTL = 0
2788     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2789     Wait M_In(11347) = 1        'toRBT_継続　の指示待ち  M5347
2790     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2791     '
2792     If M_Svo=0 Then             'サーボON確認
2793         Servo On
2794     EndIf
2795     Wait M_Svo=1
2796     Exit Function
2797 FEnd
2798 '
2799 '■ fnWindScreenOpen
2800 ''' <summary>
2801 ''' ウィンド画面の表示、非表示設定
2802 ''' </summary>
2803 '''<param name="%"></param>
2804 '''<param name="%"></param>
2805 '''<param name="%"></param>
2806 '''<param name="%"></param>
2807 ''' <remarks>
2808 ''' コメントD1001, D1002, D1003の設定
2809 ''' MWindReSet = 0     画面非表示
2810 ''' MWindInfoScr = 5   インフォメーション画面 D1003のみ
2811 ''' MWindErrScr = 10    エラー画面 D1001, D1002
2812 ''' MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
2813 ''' Date   : 2021/07/07 : M.Hayakawa
2814 ''' </remarks>
2815 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2816     If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
2817         M_Out16(12480) = MCommentD1001            'D1001 コメント
2818     EndIf
2819     '
2820     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
2821         M_Out16(12496) = MCommentD1002            'D1002 コメント
2822     EndIf
2823     '
2824     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
2825        M_Out16(12512) = MCommentD1003            'D1003 コメント
2826     EndIf
2827     '
2828     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
2829     M_Out(12363) = 1                         'ウィンド画面設定  M6362
2830     Dly 0.5
2831     M_Out(12363) = 0                         'ウィンド画面設定
2832     Exit Function
2833 FEnd
2834 '
2835 '■FnCtlValue2
2836 ''' <summary>
2837 ''' 投入数、組立OK数、組立NG数、吸着エラー数　Read/Write
2838 ''' </summary>
2839 ''' <param name="MCtlNo%"></param>
2840 ''' <remarks>
2841 ''' Date : 2022/04/28 渡辺
2842 ''' </remarks>
2843 '''
2844 '''  1：投入数       ＋１
2845 '''  2：組立ＯＫ数   ＋１
2846 '''  3：供給機２吸着エラー数 ＋１　　組立NGから変更 2022/05/19 渡辺
2847 '''  4：供給機１吸着エラー数 ＋１
2848 ''' 99：読書開始信号 OFF
2849 '''
2850 Function M% FnCtlValue2(ByVal MCtlNo%)
2851     FnCtlValue2 = 1
2852     Select MCtlNo%
2853         Case 1        '投入数＋１
2854             M_Out(12569) = 0             '書込み開始信号OFF
2855             M_Out(12568) = 1             '読込み開始信号ON
2856             MInputQty = M_In16(11600)    '投入数受信
2857             MInputQty = MInputQty + 1    '投入数＋１
2858             M_Out16(12592) = MInputQty   '投入数送信
2859             M_Out(12569) = 1             '書込み開始信号ON
2860             Break
2861             '
2862         Case 2        '組立ＯＫ数＋１
2863             M_Out(12569) = 0             '書込み開始信号OFF
2864             M_Out(12568) = 1             '読込み開始信号ON
2865             MAssyOkQty = M_In16(11616)   '組立OK数受信
2866             MAssyOkQty = MAssyOkQty + 1  '組立OK数＋１
2867             M_Out16(12608) = MAssyOkQty  '組立OK数送信
2868             M_Out(12569) = 1             '書込み開始信号ON
2869             Break
2870             '
2871         Case 3        '供給機２吸着エラー数＋１
2872             M_Out(12569) = 0                       '書込み開始信号OFF
2873             M_Out(12568) = 1                       '読込み開始信号ON
2874             MSuctionErrQty = M_In16(11632)         '供給機２吸着エラー数受信
2875             MSuctionErrQty = MSuctionErrQty + 1    '供給機２吸着エラー数＋１
2876             M_Out16(12624) = MSuctionErrQty        '供給機２吸着エラー数送信
2877             M_Out(12569) = 1                       '書込み開始信号ON
2878             Break
2879             '
2880         Case 4        '供給機１吸着エラー数＋１
2881             M_Out(12569) = 0                       '書込み開始信号OFF
2882             M_Out(12568) = 1                       '読込み開始信号ON
2883             MSuctionErrQty = M_In16(11648)         '供給機１吸着エラー数受信
2884             MSuctionErrQty = MSuctionErrQty + 1    '供給機１吸着エラー数＋１
2885             M_Out16(12640) = MSuctionErrQty        '供給機１吸着エラー数送信
2886             M_Out(12569) = 1                       '書込み開始信号ON
2887             Break
2888             '
2889         Case 99        '読書開始信号OFF
2890             M_Out(12568) = 0        '読込み開始信号OFF
2891             M_Out(12569) = 0        '書込み開始信号OFF
2892             Break
2893             '
2894     End Select
2895     Exit Function
2896 FEnd
2897 '
2898 '
2899 '■FnScreEroorCord
2900 ''' 電動ドライバーのエラーコードを含めたコメントを出す為のコメント番号の作成
2901 ''' 新規作成：2022/05/23 : 渡辺
2902 '''
2903 Function M% FnScreEroorCord()
2904     MScrewErrorCord% = 0
2905     If M_In(11252) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 1    '11252:E_driver Error Massage1 E1
2906     If M_In(11253) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 2    '11253:E_driver Error Massage1 E2
2907     If M_In(11254) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 4    '11254:E_driver Error Massage1 E3
2908     If M_In(11255) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 8    '11255:E_driver Error Massage1 E4
2909     If M_In(11258) = 1 Then MScrewErrorCord% = MScrewErrorCord% + 16   '11258:E_driver Error Massage1 E5
2910     MScrewErrorCord% = MScrewErrorCord% * 10
2911     MScrewErrorCord% = MScrewErrorCord% + 500
2912     FnScreEroorCord = MScrewErrorCord%
2913     Exit Function
2914 FEnd
2915 '
2916 '
2917 'Insightによる画像処理検査実行（並列処理なし）
2918 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2919 '-------------------------------------------------------------------------------
2920 'Insightによる画像処理検査実行（並列処理なし）
2921 '   引数
2922 '       PInspPos()      ：検査位置
2923 '       MInspGrNum%()   ：検査位置での検査グループ番号（=0：画像検査未実施）
2924 '           PInspPos()、MInspGrNum%()は同じ添え字（検査Step）のものがペア
2925 '       MInspCnt%       ：検査位置数
2926 '       MZAxis%         ：終了時のZ軸退避座標（-1:無効）
2927 '                           終了時にZ軸をMZAxisで設定された位置まで上昇させる
2928 '       MNgContinue%    ：=1で検査エラー・NG発生時に全Stepの検査を行う
2929 '   戻り値：整数
2930 '       0=異常終了、1=正常終了
2931 '
2932 '   MInspErrNum     ：異常終了時にエラー番号が設定される
2933 '   MInspNGStepNum  ：検査NG発生時の検査グループ番号が設定される
2934 '                       複数エラー発生の場合、1回目のエラー番号、検査グループ番号を設定
2935 '   20190820    :   引数 MZAxis%,MNgContinue 追加
2936 '   20200410    :   検査グループ設定Retry追加
2937 '-------------------------------------------------------------------------------
2938     '----- 初期設定 -----
2939     Cnt 0                                                           '移動効率化解除(初期値=0)
2940     Fine 0.05,P                                                     '位置決め完了条件設置　0.05mm
2941 '    Cnt 1,0.1,0.1
2942     '変数宣言・初期化
2943     Def Inte MNum                                                   '検査番号(検査順1～)
2944     MNum% = 1                                                       '検査番号初期値設定
2945     Def Inte MEndFlg                                                '検査終了フラグ
2946     MEndFlg% = 0
2947     '
2948     '検査G番号設定要求・検査実行要求off
2949     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '検査G番号設定要求off
2950     M_Out( MOUT_IS_Insp% ) = 0                                      '検査実行要求off
2951     'エラー番号クリア
2952     MInspErrNum = 0                                                 '検査実行エラー番号
2953     M_Out16(MOUT_InspErrNum) = MInspErrNum
2954     MInspNGStepNum = 0                                              '検査実行NGStep番号
2955     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2956     '
2957     'Insight Ready check?
2958     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready offなら終了
2959         MInspErrNum = 20                                            '検査実行エラー番号 20 Insight offline
2960         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2961         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2962         ISInspectionSingle = 0                                      '異常終了戻り値設定
2963         Exit Function
2964     EndIf
2965     '
2966     '検査位置数確認
2967     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2968         MInspErrNum = 21                                            '検査データなし 21　引数<1
2969         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
2970         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
2971         ISInspectionSingle = 0                                      '異常終了戻り値設定
2972         Exit Function
2973     EndIf
2974     '
2975     '
2976     '
2977     '----- メイン処理 -----
2978     '設定された検査位置数分の検査実行
2979     While( MEndFlg% = 0 )
2980         '----- 検査グループ番号設定Retry追加 20200410
2981         MSetGrNumRetryExitFlg = 0
2982         MSetGrNumRetryCnt = 2                                           'Retry回数設定
2983         While( MSetGrNumRetryExitFlg = 0 )
2984         '----- 検査グループ番号設定Retry追加ここまで 20200410
2985             '
2986             MCurrentStepErr = 0                                         '現Step検査エラーフラグリセット
2987             '
2988             '----- 検査グループ番号設定 -----
2989             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '検査G番号設定
2990             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '検査G番号設定要求on
2991             '
2992             '検査位置へ移動・移動完了待ち
2993             fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
2994             Mvs PInspPos( MNum% )                                       '移動
2995             fnAutoScreenComment(523)                                    '状態表示[画像処理検査中] 2022/05/09 渡辺
2996             Dly 0.05                                                    '移動完了後Delay
2997             '
2998             '検査グループ番号設定終了確認
2999             M_Timer(1) = 0
3000             MExitFlg = 0
3001             While( MExitFlg = 0 )
3002                 '検査G設定正常終了?
3003                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
3004                     MExitFlg = 1
3005                 '
3006                 '検査G設定異常終了?
3007                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
3008                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
3009                     If MInspErrNum = 0 Then                             '1回目のエラー?
3010                         MInspErrNum = 14                                '検査G設定異常 エラー番号=14
3011                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
3012                     EndIf
3013                     MExitFlg = 1
3014                 '
3015                 'timeoutチェック
3016                 ElseIf 1000 < M_Timer(1) Then
3017                     MCurrentStepErr = 1                                 '現Step検査エラーフラグ
3018                     If MInspErrNum = 0 Then                             '1回目のエラー?
3019                         MInspErrNum = 12                                'timeout エラー番号=12
3020                         MInspNGStepNum = MInspGrNum%(MNum%)             'エラー検査G番号設定
3021                     EndIf
3022                     MExitFlg = 1
3023                 EndIf
3024             WEnd
3025             '
3026             '検査G番号設定要求off
3027             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '検査G番号設定要求off
3028             '
3029             '----- 検査グループ設定Retry追加 20200410
3030             'NGなければ抜ける
3031             If MCurrentStepErr = 0 Then
3032                 MSetGrNumRetryExitFlg = 1
3033             Else
3034                 'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
3035                 If MSetGrNumRetryCnt = 0 Then
3036                     MSetGrNumRetryExitFlg = 1
3037                 Else
3038                     'Retryへ　その前にDelay
3039                     Dly 0.5
3040                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
3041                 EndIf
3042             EndIf
3043             '----- 検査グループ設定Retry追加ここまで 20200410
3044             '
3045         WEnd
3046         '
3047         '
3048         '
3049         '----- 検査実行 -----
3050         If MCurrentStepErr = 0  Then                                '検査G番号設定NGの場合は検査実行しない
3051             If 0 < MInspGrNum%(MNum%) Then                          '検査あり?
3052                 MJudgeOKFlg = 0                                     '検査OKフラグクリア
3053                 MInspRetryExitFlg = 0
3054                 MRetryCnt = 2                                        'Retry回数設定
3055                 While( MInspRetryExitFlg = 0 )
3056                     M_Out( MOUT_IS_Insp% ) = 1                      '検査実行要求on
3057                     '
3058                     '検査完了確認
3059                     MRetryCnt = MRetryCnt - 1
3060                     M_Timer(1) = 0
3061                     MExitFlg = 0
3062                     While( MExitFlg = 0 )
3063                     '検査完了待ち
3064                         '検査OK終了?
3065                         If M_In( MIN_IS_InspOK% ) = 1  Then
3066                             MJudgeOKFlg = 1                         '検査OKフラグON
3067                             MExitFlg = 1
3068                         '
3069                         '検査NG終了?
3070                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
3071                             If MInspErrNum = 0 Then                 '1回目のエラー?
3072                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
3073                                     MInspErrNum = 32                    '検査NG エラー番号=32
3074                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
3075                                 EndIf
3076                             EndIf
3077                             MExitFlg = 1
3078                         '
3079                         '検査異常終了(IS timeout)?
3080                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
3081                             If MInspErrNum = 0 Then                 '1回目のエラー?
3082                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
3083                                     MInspErrNum = 38                    '検査異常終了 エラー番号=38
3084                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
3085                                 EndIf
3086                             EndIf
3087                             MExitFlg = 1
3088                         '
3089                         'timeoutチェック
3090                         ElseIf 3000 < M_Timer(1) Then
3091                             If MInspErrNum = 0 Then                 '1回目のエラー?
3092                                 If MRetryCnt = 0 Then               'RetryしてもNGならNGとする
3093                                     MInspErrNum = 34                    '検査異常終了 エラー番号=34
3094                                     MInspNGStepNum = MInspGrNum%(MNum%) 'エラー検査G番号設定
3095                                 EndIf
3096                             EndIf
3097                             MExitFlg = 1
3098                         EndIf
3099                     WEnd
3100                     '
3101                     '検査開始要求off
3102                     M_Out(MOUT_IS_Insp%) = 0                        '検査実行要求off
3103                     '
3104                     'OKなら抜ける
3105                     If MJudgeOKFlg = 1 Then
3106                         MInspRetryExitFlg = 1
3107                     Else
3108                         'Retry回数終了でもNG判定(OKフラグoffなら抜ける)
3109                         If MRetryCnt = 0 Then
3110                             MInspRetryExitFlg = 1
3111                         Else
3112                             'Retryへ　その前にDelay
3113                             Dly 0.3
3114                         EndIf
3115                     EndIf
3116                     '
3117                 WEnd
3118             EndIf
3119         EndIf
3120         '
3121         '
3122         '
3123         MNum% = MNum% + 1                                           '検査Step+1
3124         '検査終了確認　検査終了フラグセット
3125         If (MInspCnt% < MNum% ) Then
3126             MEndFlg% = 1                                            '検査終了フラグセット
3127         EndIf
3128         'NG発生時続行時処理
3129         If MInspErrNum <> 0 Then                                    'NGあり?
3130             If MNgContinue% <> 1 Then                               'NG続行?
3131                 MEndFlg% = 1                                        '検査終了フラグセット
3132             EndIf
3133         EndIf
3134     WEnd
3135     '
3136     '終了時にZ軸をMZAxisで設定された位置まで上昇させる
3137     If 0 < MZAxis% Then
3138         PCurrentPos = P_Curr                                        '現在位置取得
3139         PCurrentPos.Z = MZAxis%                                     'Z軸を設定
3140         fnAutoScreenComment(521)                                    '状態表示[６軸ロボ動作中] 2022/05/09 渡辺
3141         Mvs PCurrentPos                                             '現在位置上空へ移動
3142     EndIf
3143     '
3144     '戻り値設定
3145     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      'カメラ検査強制OK(M_In(11372)=1)追加(12/21中村)
3146         ISInspectionSingle = 1                                      '正常終了戻り値設定
3147     Else
3148         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '検査実行エラー番号出力
3149         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '検査実行NGStep番号出力
3150         ISInspectionSingle = 0                                      '異常終了戻り値設定
3151     EndIf
3152     Fine 0 , P
3153     Exit Function
3154 FEnd
3155 '
3156 '■InitialZoneB
3157 ''' <summary>
3158 ''' 非常停止後の復帰動作
3159 ''' 1)上空退避　Z方向上に移動
3160 ''' 2)J1軸以外を退避ポジションへ移動
3161 ''' 3)J1軸のみを退避ポジションへ移動
3162 ''' 4)イニシャルポジションへ移動
3163 ''' </summary>
3164 ''' <remarks>
3165 ''' Date : 2022/04/12 : N.Watanabe
3166 ''' </remarks>
3167 Function V fnInitialZoneB()
3168     fnAutoScreenComment(520)    '状態表示[６軸ロボ初期位置移動中] 2022/05/09 渡辺
3169 'パラメータ
3170     Ovrd 5
3171 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
3172 '    Cmp Pos, &B100011
3173 '
3174 '復帰動作開始
3175 '
3176 '置き台と両掴みの場所は、チャックを解放する
3177 *RecoveryChuckOpen
3178     PActive = P_Curr          '現在位置を取得
3179     MRecoveryChuckOpen = 0    'チャック解放フラグ 初期化
3180 'PProductOnJigSet(製品置き位置)は、チャック解放
3181     If (PActive.X <= PProductOnJigSet.X + 1.0) And (PActive.X >= PProductOnJigSet.X -1.0) Then
3182         If (PActive.Y <= PProductOnJigSet.Y + 1.0) And (PActive.Y >= PProductOnJigSet.Y -1.0) Then
3183             If (PActive.Z <= PProductOnJigSet.Z + 1.0) And (PActive.Z >= PProductOnJigSet.Z -1.0) Then
3184                 MRecoveryChuckOpen = 1
3185             EndIf
3186         EndIf
3187     EndIf
3188 'PProductOnJigGet(製品取り出し位置)は、チャック解放
3189     If (PActive.X <= PProductOnJigGet.X + 1.0) And (PActive.X >= PProductOnJigGet.X -1.0) Then
3190         If (PActive.Y <= PProductOnJigGet.Y + 1.0) And (PActive.Y >= PProductOnJigGet.Y -1.0) Then
3191             If (PActive.Z <= PProductOnJigGet.Z + 1.0) And (PActive.Z >= PProductOnJigGet.Z -1.0) Then
3192                 MRecoveryChuckOpen = 1
3193             EndIf
3194         EndIf
3195     EndIf
3196     If MRecoveryChuckOpen = 1 Then
3197         M_Out(12256) = 0        'チャック閉OFF
3198         M_Out(12257) = 1        'チャック開ON
3199         M_20# = 0               'KEY入力初期化
3200         MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'チャック開検出
3201         If MRtn = 0 Then
3202             fErrorProcess(11,244,284,0)
3203             If M_20# = MNext% Then M_20# = MClear%
3204             If M_20# = MAbout% Then GoTo *RecoveryEnd
3205             If M_20# = MNgProcess% Then GoTo *RecoveryEnd
3206             If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
3207         Else
3208             M_Out(12257) = 0        'チャック開OFF
3209         EndIf
3210     EndIf
3211 '
3212 '上空退避
3213     PActive = P_Curr
3214     Pmove = PActive
3215     Pmove.Z = 600           '上空退避する一律の高さ
3216     If PActive.X > 400 Then
3217         Pmove.Z =400        'パレット上に腕を伸ばしているときは640まで上げられない為、例外処置
3218     EndIf
3219     If PActive.Z < Pmove.Z Then '現在の高さがPmoveより低い時のみ実行
3220         Mvs Pmove
3221     EndIf
3222     Dly 1.0
3223 'J1軸以外を退避ポジションへ移動
3224     JActive = J_Curr
3225     Jmove = JTaihi
3226     Jmove.J1 = JActive.J1        'J1軸のみ現在値を使用し、他の軸はJTaihiのポーズを取る
3227     Mov Jmove
3228     Dly 1.0
3229 'J1軸のみを退避ポジションへ移動
3230     Mov JTaihi
3231     Dly 1.0
3232 'イニシャルポジションへ移動
3233     Mov PInitialPosition
3234     Cmp Off
3235     Ovrd 100
3236     M_Out(12268) = 0            '位置決め出OFF
3237     M_Out(12269) = 1            '位置決め戻ON
3238     fErrorProcess(11,253,281,0)
3239     Exit Function
3240 *RecoveryEnd
3241 FEnd
3242 '
3243 '
3244 '■fnAutoScreenComment
3245 ''' <summary>
3246 ''' メイン画面の動作状況表示
3247 ''' コメントD1005の設定
3248 ''' </summary>
3249 '''<param name="McommentD1005%">コメントID</param>
3250 ''' <remarks>
3251 ''' Date   : 2021/07/07 : M.Hayakawa
3252 ''' </remarks>
3253 Function fnAutoScreenComment(ByVal McommentD1005%)
3254     M_Out16(12576) = McommentD1005%
3255     Exit Function
3256 FEnd
3257 '
3258 '■fnRoboPosChk
3259 ''' <summary>
3260 ''' 最後に終了したロボットポジションの確認
3261 ''' </summary>
3262 '''<param name="MINNumber%">入力番号</param>
3263 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3264 '''<param name="MTimeCnt&">タイムアウト時間</param>
3265 ''' PLCに保続した番号を読込み、確認
3266 ''' MRBTOpeGroupNo = 5 が初期位置に設定
3267 '''<returns>整数 0:タイムアウト 1:OK</returns>
3268 ''' <remarks>
3269 ''' Date   : 2021/07/07 : M.Hayakawa
3270 ''' </remarks>
3271 Function M% fnRoboPosChk
3272     fnRoboPosChk = 0
3273     MRet = fnStepRead()
3274     '初期位置でないと判断した場合
3275     'ウィンド画面切換え
3276     If MRBTOpeGroupNo > 5 Then
3277         '下記キー待ちの継続に反応させないため
3278         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3279         Dly 0.2
3280         Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3281         Dly 1.5
3282         '
3283         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  'ウィンド画面エラー表示とコメント設定
3284         '
3285         MLoopFlg% = 1
3286         While MLoopFlg% = 1
3287             '
3288             '
3289             MKeyNumber% = fnKEY_WAIT()
3290             Select MKeyNumber%
3291                 Case Is = MAbout%       '停止
3292                     M_20# = MAbout%
3293                     MLoopFlg% = -1
3294                     Break
3295                 Case Is = MNext%        '次へ
3296                     'MLoopFlg% = -1
3297                     Break
3298                 Case Is = MContinue%    '継続
3299                     M_20# = MContinue%
3300                     MLoopFlg% = -1
3301                     Break
3302                 Default
3303                     Break
3304             End Select
3305         WEnd
3306     EndIf
3307     '
3308     If M_20# = MContinue% Then                              '継続ボタンが押された場合
3309         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   'ウィンド画面エラー表示とコメント設定
3310         Ovrd 5                                   '低速オーバーライド値設定
3311         Select MRBTOpeGroupNo
3312             Case Is = 5                          '何もしない
3313                 Break
3314             Case Is = 10                         '初期位置へ戻す
3315                 'Mov PTEST001
3316                 Break
3317             Case Is = 15                         '初期位置へ戻す
3318                 'Mov PTEST002
3319                 Dly 0.5
3320                 'Mov PTEST001
3321                 Dly 0.5
3322                 Break
3323             Default
3324                 Break
3325         End Select
3326         '
3327         Ovrd M_NOvrd                            'システムの初期値を設定
3328         M_Out(12364) = 1                        'toPLC_データ保存ON
3329         MRBTOpeGroupNo = 5
3330         MRet = fnStepWrite(MRBTOpeGroupNo)      '初期位置の番号転送
3331         Dly 1.0
3332         M_Out(12364) = 0                        'toPLC_データ保存OFF
3333         fnRoboPosChk = 1                        '初期位置動作実行
3334         fnWindScreenOpen(MWindReSet,  0, 0, 10)  'ウィンド画面エラー表示とコメント設定
3335     EndIf
3336     Exit Function
3337 FEnd
3338 '
3339 '■frInCheck
3340 ''' <summary>
3341 ''' センサーINチェック
3342 ''' </summary>
3343 '''<param name="MINNumber%">入力番号</param>
3344 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3345 '''<param name="MTimeCnt&">タイムアウト時間</param>
3346 '''<returns>整数 0:タイムアウト 1:OK</returns>
3347 ''' <remarks>
3348 ''' Date   : 2021/07/07 : M.Hayakawa
3349 ''' </remarks>
3350 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3351     M_Timer(4) = 0
3352     MloopFlg = 0
3353     While MloopFlg = 0
3354         MCrtTime& = M_Timer(4)
3355         If M_In(MINNumber%) = MCMPFLG% Then
3356             MloopFlg = 1
3357             frInCheck = 1
3358         ElseIf MCrtTime& > MTimeCnt& Then
3359             MloopFlg = 1
3360             frInCheck = 0
3361         EndIf
3362     WEnd
3363     Exit Function
3364 FEnd
3365 '■frScrewInCheck
3366 ''' <summary>
3367 ''' ねじ吸着センサーINチェック
3368 ''' </summary>
3369 '''<param name="MINNumber%">入力番号</param>
3370 '''<param name="MCMPFLG%">0:OFF確認 1:ON確認</param>
3371 '''<param name="MTimeCnt&">タイムアウト時間</param>
3372 '''<returns>整数 0:タイムアウト 1:OK</returns>
3373 ''' <remarks>
3374 ''' Date   : 2022/07/10 : 中村天哉
3375 ''' </remarks>
3376 Function M% frScrewInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3377     M_Timer(4) = 0
3378     MloopFlg = 0
3379     While MloopFlg = 0
3380         MCrtTime& = M_Timer(4)
3381         If M_In(MINNumber%) = MCMPFLG% Then
3382             Dly 0.05
3383             If M_In(MINNumber%) = MCMPFLG% Then
3384                 MloopFlg = 1
3385                 frScrewInCheck = 1
3386             EndIf
3387         ElseIf MCrtTime& > MTimeCnt& Then
3388             MloopFlg = 1
3389             frScrewInCheck = 0
3390         EndIf
3391     WEnd
3392     Exit Function
3393 FEnd
3394 '-----------------------------------------------
3395 '
3396 'ねじ締め機通信確認
3397 '
3398 '-----------------------------------------------
3399 Function M% fScewTcomChk
3400     fScewTcomChk = 0
3401     '通信確認送信
3402     M_Out(MOUT_ScwT_ComChk%) = MOn%
3403     '通信確認受信待機
3404     Wait M_In(MIN_ScwT_comOK%) = MOn%
3405     '通信確認送信終了
3406     M_Out(MOUT_ScwT_ComChk%) = MOff%
3407     Exit Function
3408 FEnd
3409 '
3410 '
3411 '-----------------------------------------------
3412 '
3413 'ねじ締め開始送信
3414 '
3415 '-----------------------------------------------
3416 Function M% fScewTStart
3417     fScewTStart = 0
3418     'ねじ締め開始待機を受信
3419     Wait M_In(MIN_ScwT_STRec%) = MOn%
3420     Dly 0.1
3421     'ねじ締め開始受信を送信
3422     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msecパルス
3423     Exit Function
3424 FEnd
3425 '
3426 '
3427 '-----------------------------------------------
3428 '
3429 'ねじ締め完了受信
3430 '
3431 '-----------------------------------------------
3432 Function M% fScewTFinish
3433     fScewTFinish = 0
3434     'ねじ締め完了待機を受信
3435     Wait M_In(MIN_ScwT_Fin%) = MOn%
3436     Dly 0.1
3437     'ねじ締め完了受信を送信
3438     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msecパルス
3439     Exit Function
3440 FEnd
3441 '
3442 '
3443 '-----------------------------------------------
3444 '
3445 '条件xx停止受信
3446 '
3447 '-----------------------------------------------
3448 Function M% fScewTCaseStop(ByVal MCase%())
3449     fScewTCaseStop = 0
3450     '条件xx停止を受信
3451     Wait M_In(MCase%(1)) = MOn%
3452     Dly 0.1
3453     '条件xx停止受信を送信
3454     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msecパルス
3455     Exit Function
3456 FEnd
3457 '
3458 '-----------------------------------------------
3459 '
3460 '再開始受信
3461 '
3462 '-----------------------------------------------
3463 Function M% fScewTReStart()
3464     fScewTReStart = 0
3465     '再開始を受信
3466     Wait M_In(MIN_ScwT_ReST%) = MOn%
3467     Dly 0.1
3468     '再開始受信を送信
3469     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msecパルス
3470     Exit Function
3471 FEnd
3472 '
3473 '■fErrorProcess
3474 '<summary>
3475 'エラー処理
3476 '</summary>
3477 '<param name = "MErrorScreenNo%"> スクリーン番号</param>
3478 '<param name = "MErrorCommentD1001%"> D1001コメント番号 </param>
3479 '<param name = "MErrorCommentD1002%"> D1002コメント番号 </param>
3480 '<param name = "MErrorCommentD1003%"> D1003コメント番号 </param>
3481 '<make>
3482 '2021/11/5 中村天哉
3483 '</make>
3484 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3485     MScreenNo = MErrorScreenNo%                    'エラースクリーン番号
3486     MCommentD1001 = MErrorCommentD1001%            'D1001コメント番号
3487     MCommentD1002 = MErrorCommentD1002%            'D1002コメント番号
3488     MCommentD1003 = MErrorCommentD1003%            'D1003コメント番号
3489 *RETRY_ERR_PROCESS
3490      M_20# = MClear%     '初期化
3491 '        'エラー処理記述
3492         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3493 '        'GOT KEY入力待ち
3494         MKeyNumber = fnKEY_WAIT()
3495 '        '
3496         If MKeyNumber = MAbout% Then   '停止を選択した場合
3497             M_20# = MAbout%            'M_20# プログラム間共通外部変数
3498             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3499             Break
3500          '
3501         ElseIf MKeyNumber = MContinue% Then   '継続を選択した場合
3502             M_20# = MContinue%            'M_20# プログラム間共通外部変数
3503             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3504         '
3505         ElseIf MKeyNumber = MNext% Then   '次へを選択した場合
3506             M_20# = MNext%            'M_20# プログラム間共通外部変数
3507             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3508          '
3509         ElseIf MKeyNumber = MNgProcess% Then   '停止を選択した場合
3510             M_20# = MNgProcess%            'M_20# プログラム間共通外部変数
3511             fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3512             Break
3513         '
3514         EndIf
3515         '
3516         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3517     Exit Function
3518 FEnd
3519 '
3520 '■fnTorqueCheck
3521 ''' <summary>
3522 ''' トルクチェック動作用のメイン
3523 ''' </summary>
3524 ''' <remarks>
3525 ''' Date   : 2021/12/21 : H.AJI
3526 ''' </remarks>'
3527 Function M% fnTorqueCheck
3528     'トルクチェック中送信  搬送系停止
3529     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLCへトルクチェック中を送信
3530     '
3531     fnTorqueCheck = 0
3532     Ovrd 20
3533     Mov PInitialPosition              '初期位置移動
3534     Ovrd 100
3535     '下記キー待ちの継続に反応させないため
3536     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち
3537     Dly 0.2
3538     Wait M_In(11347) = 0                 'toRBT_継続の完了待ち　2重確認
3539     '
3540     'M6340  トルクチェック受信
3541     'Dly 5.0
3542     M_Out(12340) = 1          'トルクチェック受信 M6340
3543     Dly 1.0
3544     M_Out(12340) = 0
3545     '
3546     MRet = fnMainScreenOpen(11, 60, 61, 0)   'トルクチェック画面表示
3547     '
3548     MLoopFlg = 1
3549     While MLoopFlg = 1
3550         '
3551         Mov PInitialPosition              '初期位置移動
3552         '
3553         MKeyNumber = fnKEY_WAIT()
3554         Select MKeyNumber
3555             Case Is = 1           '停止
3556                 M_Out(12343) = 1          '停止要求開始要求受信 M6343
3557                 Dly 1.0
3558                 M_Out(12343) = 0
3559                 Ovrd 20
3560                 Mov PTicketRead_1
3561                 Ovrd 100
3562                 M_20# = 1
3563                 MLoopFlg = -1
3564                 Break
3565             Case Is = 2           '次へ
3566                 Break
3567             Case Is = 3           '継続
3568                 Break
3569             Case Is = 4           'トルクチェック開始
3570                 M_Out(12545) = 1    ' toPLC_PCトルクチェック1要求受信(M315)
3571                 M_Out(12342) = 1 Dly 1.0    'トルクチェック開始要求受信 M6342
3572                 fnWindScreenOpen(29,  0, 0, 0)  'ウィンド画面エラー表示とコメント設定
3573                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  'ウィンド画面エラー表示とコメント設定
3574                 MRet = fnMoveTorquePosi()
3575                 'MRet = fnAutoScreenComment(67)  'AUTO画面 通過履歴NG書込み
3576                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  'エラー画面消去
3577                 Break
3578             Default
3579                 Break
3580         End Select
3581     WEnd
3582     '
3583     'トルクチェック中停止送信
3584     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLCへトルクチェック中を送信
3585     '
3586     'ロボットの位置を元に戻す
3587     '
3588     Exit Function
3589  FEnd
3590  '
3591 '
3592 '
3593 '---------------------------
3594 '
3595 '    メイン画面の表示、非表示設定
3596 '         コメントD1001, D1002, D1003の設定
3597 '           MWindReSet = 0     画面非表示
3598 '           MWindInfoScr = 5   インフォメーション画面 D1003のみ
3599 '           MWindErrScr = 10    エラー画面 D1001, D1002
3600 '           MWindCmmnScr = 20   エラー以外のコメント画面 D1001, D1002
3601 '
3602 '---------------------------
3603 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3604     fnMainScreenOpen = 0
3605     '
3606    If MCommentD1001 <> 0 Then                    'コメント 0 は設定がないので確認
3607         M_Out16(12480) = MCommentD1001            'D1001 コメント
3608     EndIf
3609     '
3610     If MCommentD1002 <> 0 Then                    'コメント 0 は設定がないので確認
3611         M_Out16(12496) = MCommentD1002            'D1002 コメント
3612     EndIf
3613     '
3614     If MCommentD1003 <> 0 Then                    'コメント 0 は設定がないので確認
3615         M_Out16(12512) = MCommentD1003            'D1003 コメント
3616     EndIf
3617     '
3618     M_Out16(12448) = MScreenNo                '画面番号  M6448   10=エラー画面
3619     M_Out(12362) = 1                         'ウィンド画面設定  M6362
3620     Dly 0.5
3621     M_Out(12362) = 0                         'ウィンド画面設定
3622     Exit Function
3623 FEnd
3624 '
3625 '■Main
3626 ''' <summary>
3627 ''' トルクチェック実動作
3628 ''' </summary>
3629 ''' <remarks>
3630 ''' Date   : 2021/12/21 : H.AJI
3631 ''' </remarks>'
3632 Function M% fnMoveTorquePosi
3633      fnMoveTorquePosi = 0
3634      Ovrd 50
3635      Mov PTorqueCheck_1 'トルクチェックメーター上空へ移動
3636     '
3637     Spd M_NSpd
3638 '-------------      ドライバーRST
3639     M_Out(12240)=0     'ドライバーOFF CCW
3640     M_Out(12241)=0     'ドライバーOFF CW
3641     M_Out(12242)=0     'ドライバー解除 C1
3642     M_Out(12243)=0     'ドライバー解除 C2
3643     M_Out(12245)=0     'プログラム解除 F1/プログラム2
3644 '---------------------------------------
3645 '[P-11]
3646 '--------------------------------------------------------------   【トルクチェック 0.4N - P11】
3647     Mov PTorqueCheck, -50                     ' トルク-1　置き位置上空 50mm へ移動
3648     Dly 0.1
3649 '-----------------------
3650    'Cnt 0                           'Cnt動作-2　終了
3651 '-----------------------
3652     Mov PTorqueCheck , -5                      'トルク-1　置き位置上空 5mm へ移動
3653     Dly 0.2
3654 '-----------------------
3655     ProgramBankSet(1,3)
3656     M_Out(12241)=0                   'ドライバーOFF  CW
3657     'Dly 0.1
3658 '--------------------------------
3659     Ovrd 40
3660    'Dly 0.1
3661 '--------------------------------  ネジ締め速度設定
3662     Spd 14                            'ライド 100-40 100% :Spd 12
3663     Dly 0.1
3664 '--------------------------------
3665 '--------------------------------
3666 '---------------------------------【ねじ締め動作】
3667 '
3668     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '移動中エラー検出
3669    Mvs PTorqueCheck               'トルクチェック位置へ移動
3670     Dly 0.3                          '動作安定待ち
3671    M_Out(12241)=1                   'ドライバーON  CW
3672 '
3673     Wait M_In(11584)=1                '完了/エラー検出
3674     Dly 0.1
3675     Spd M_NSpd
3676    'Ovrd 20
3677     If M_In(11256)=1 Then *LBL1       'ネジトータルエラー検出
3678     Wait M_In(11257)=1                'ネジ完了SC
3679 '---------------------------------
3680     Dly 0.1
3681     M_Out(12241)=0                    'ドライバーOFF CW
3682     Dly 0.1
3683     M_Out(12242)=0                    'ドライバー解除 C1
3684     Dly 0.1
3685     M_Out(12243)=0                    'ドライバー解除 C2 (バンク3)
3686     Dly 0.1
3687     M_Out(12245)=0                    'プログラム2解除 F1
3688 '--------------------------------------------------------------   【トルクチェック 0.4N - P11ここまで】
3689 '
3690     Mvs PTorqueCheck,-60                       'あえてmov から変更
3691     Dly 0.1
3692 '--------------------------------------------------------------
3693    'Ovrd 80
3694 '--------------------------------------------------------------
3695 '---------------------------------------
3696 '---------------------------------------
3697 '---------------------------------------エラー離脱処理
3698    *LBL1
3699    Fsc Off            '力覚センサ　Off   *STEP1は不要
3700    Mvs ,-100
3701    M_Out(12241)=0     'ドライバーOFF CW
3702    Dly 0.1
3703    M_Out(12242)=0     'ドライバー解除 C1
3704    Dly 0.1
3705    M_Out(12243)=0     'ドライバー解除 C2 (バンク3)
3706    Dly 0.1
3707    M_Out(12245)=0     'プログラム解除 F1
3708 '---------------------------------------
3709 '---------------------------------------
3710 '-------------
3711    'Mov PInitPos19049
3712    Dly 0.1
3713 '
3714 '
3715     Exit Function
3716 FEnd
3717 '
3718 '■Main
3719 ''' <summary>
3720 ''' 組立動作用のメイン
3721 ''' </summary>
3722 ''' <remarks>
3723 ''' Date   : 2021/07/07 : M.Hayakawa
3724 ''' </remarks>'
3725 Function Main
3726     MopeNo = M_21#         '外部変数にて動作番号代入
3727     '
3728     If M_Svo=0 Then
3729         Servo On
3730     EndIf
3731     Wait M_Svo=1
3732 '組立スタート日付時刻要求パルスON
3733     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3734 'パトライト操作
3735     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT操作権ON
3736     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT 青
3737     '
3738     M_20# = 0                                   'KEY入力初期化
3739     M_Out(MOUT_OKNG%) = 0                       '後工程へNGフラグを出力初期化
3740     MRet% = 0
3741 '初期位置の確認と移動
3742 '
3743 '復帰動作　実行・未実行判別      2022/04/12 渡辺 作成
3744     PActive = P_Curr                    '現在位置を取得
3745     MRecoveryPass% = 0
3746     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3747         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3748             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3749                 MRecoveryPass% = 1       'イニシャルポジションは復帰動作パス
3750             EndIf
3751         EndIf
3752     EndIf
3753     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3754         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3755             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3756                 MRecoveryPass% = 1       'チケット読み込み上空位置は復帰動作パス
3757             EndIf
3758         EndIf
3759     EndIf
3760     If MRecoveryPass% = 0 Then
3761        fnInitialZoneB()        '復帰動作パスフラグが立っていない時は復帰動作を実行
3762     EndIf
3763 '
3764 '
3765 '    MRet% = fnRoboPosChk()
3766     If MRet% = 1 Then                           '初期位置の動作を行った場合
3767         fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  'ウィンド画面
3768         MKeyNumber% = fnKEY_WAIT()
3769         Select MKeyNumber%
3770             Case Is = MAbout%       '停止
3771                 M_20# = MAbout%
3772                 MLoopFlg% = -1
3773                 Break
3774             Case Is = MNext%        '次へ
3775                 'MLoopFlg = -1
3776                 Break
3777             Case Is = MContinue%    '継続
3778                 M_20# = MContinue%
3779                 MLoopFlg% = -1
3780                 Break
3781             Default
3782                 Break
3783         End Select
3784     EndIf
3785     '
3786     If M_20# <> MAbout% Then        '外部変数 M_20# が 1=停止 以外の場合
3787         M_Out(12364) = 1            'toPLC_データ保存ON
3788 'トルクチェック
3789         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3790             MRet% = fnTorqueCheck()
3791             Break
3792         Else
3793 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_使用確認
3794 '                MRtn = InspInit()               '画像処理初期化処理
3795 '            EndIf
3796             '
3797            M_20# = MClear%                    '初期化
3798 '組立開始
3799             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3800 '                MRet% = fnAssyStart()
3801                 fnAssyStart()
3802             Else
3803                 M_20# = MPass%
3804             EndIf
3805 '組立終了日付時刻
3806             M_Out(MOUT_ED_DATETIME%) = 1    '組立終了日付時刻
3807             Wait M_In(11572) = 1            '日付取得完了
3808             Dly 0.1
3809             M_Out(MOUT_ED_DATETIME%) = 0    '組立終了日付時刻
3810 'リフターユニットへのOUT
3811             '  KEY入力が何もない場合 OKと判断
3812             fnAutoScreenComment(89)         'AUTO画面 組立処理完了
3813             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO画面 組立処理完了
3814 'OK/NGフラグ出力
3815             If M_20# <= 0 Then
3816                 M_Out(MOUT_OKNG%) = 1       '後工程へOKフラグを出力(PLC OUT)
3817             ElseIf M_20# = MPass% Then
3818                 M_Out(MOUT_OKNG%) = 0       '後工程へNGフラグを出力(PLC OUT)
3819             EndIf
3820 'PIASに組立完了書込み
3821             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON確認
3822                 If M_20# = MPass% Then
3823                     M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3824                 Else
3825                     'KEY入力がNGの場合
3826                     If M_20# = MNgProcess% Then
3827                         M_Out(MOUT_OKNG%) = 0                   '後工程へNGフラグを出力(PLC OUT)
3828                         fnAutoScreenComment(90)  'AUTO画面 通過履歴NG書込み
3829                         MRet% = fnPiasWrite(MNG%)
3830                        nAssyNgQty = nAssyNgQty + 1
3831                     EndIf
3832                     '
3833                     'KEY入力が何もない場合 OKと判断(MAssyOK%に変更1/17中村)
3834                     If M_20# = MAssyOK% Then
3835                             '-----------------------
3836                             'D732 -> D2600 コピー要求
3837                             M_Out(12566) = 1
3838 '                            Wait M_In(11581) = 1   'PLCよりコピー完了信号
3839                             M_Out(12566) = 0
3840                             '
3841                         If M_In(11367) = 0 Then          '基板履歴書込みキャンセル=1 DEbug用
3842                             'MRet% = fnAutoScreenComment(91)  'AUTO画面 基板情報書込み
3843                             '基板番号照合(PPは未使用）
3844 '                            MRet% = fnPCBNumberCheck()
3845                         Else
3846                             MRet% = 1
3847                         EndIf
3848                         '
3849                         If M_In(11368) = 0 Then          '工程履歴書込みキャンセル=1 DEbug用
3850                             If M_20# <> MAbout% Then
3851                                 '工程履歴OK書き込み
3852                                 M_Out(MOUT_OKNG%) = 1                   '後工程へOKフラグを出力(PLC OUT)
3853                                 fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3854                                 MRet% = fnPiasWrite(MOK%)
3855                                 nAssyOkQty = 0
3856                                 nAssyOkQty = nAssyOkQty + 1
3857                             Else
3858                                 nAssyOkQty = nAssyOkQty + 1
3859                             EndIf
3860                         EndIf
3861                     EndIf
3862 '                    fnAutoScreenComment(92)  'AUTO画面 通過履歴OK書込み
3863 '                    MRet% = fnPiasWrite(MOK%)
3864                 EndIf
3865             Else
3866                 nAssyOkQty = nAssyOkQty + 1
3867             EndIf
3868             '
3869             '組立終了日付時刻解除
3870             M_Out(MOUT_ED_DATETIME%) = 0                '組立終了日付時刻
3871             '投入数、組立OK数、組立NG数書込み
3872 '            MRtn = FnCtlValue2(2)                       '書込み 2022/04/28 コメントアウト 渡辺
3873             '
3874 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_使用確認
3875 '                '画像処理終了処理
3876 '                MRtn = InspQuit()
3877 '            EndIf
3878         EndIf
3879         M_Out(12364) = 0                          'toPLC_データ保存OFF
3880     EndIf
3881 'パトライト操作
3882     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT操作権ON
3883     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT 青
3884 'GOT表示
3885     fnAutoScreenComment(93)  'AUTO画面 工程完了
3886 FEnd
3887 End
3888 '
3889 'おまじないコメント
3890 '絶対削除するな
3891 '
3892 '
3893 '
PInspPosition(1)=(+601.26,-152.24,+374.96,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(11)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(12)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(13)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(14)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(15)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(16)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(17)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(18)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(19)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(20)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(21)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(22)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(23)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(24)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(25)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(26)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(27)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(28)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(29)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PInspPosition(30)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PTemp=(+601.26,-152.24,+450.96,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(1)=(-318.59,-106.60,+570.00,-179.73,+1.08,-89.97,+0.00,+0.00)(7,1)
PScrewPos(2)=(-318.59,-106.60,+544.46,-179.73,+1.08,-89.97,+0.00,+0.00)(7,1)
PScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(10)=(-318.59,-106.60,+538.71,-179.73,+1.08,-89.97,+0.00,+0.00)(7,1)
PGetScrewPos(1)=(-134.50,+200.27,+470.00,-180.00,+0.01,+170.74,+0.00,+0.00)(7,0)
PGetScrewPos(2)=(-134.50,+200.27,+570.00,-180.00,+0.01,+170.74,+0.00,+0.00)(7,0)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(-50.58,+239.35,+544.69,-180.00,+0.01,+170.74,+0.00,+0.00)(7,0)
PGetScrewPos(10)=(-134.50,+200.27,+451.67,-180.00,+0.01,+170.74,+0.00,+0.00)(7,0)
PEscapePosi(1)=(-252.42,-263.31,+570.00,+179.96,-0.07,-43.81)(7,1)
PEscapePosi(2)=(-141.51,-152.87,+570.00,+179.96,-0.07,-43.81)(7,1)
PEscapePosi(3)=(-208.31,+0.23,+570.00,-180.00,+0.00,-90.00)(7,0)
PEscapePosi(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PEscapePosi(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PEscapePosi(6)=(-208.31,+0.01,+610.00,+180.00,+0.00,+0.00)(7,0)
PEscapePosi(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PEscapePosi(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PEscapePosi(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PEscapePosi(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PActive=(+601.26,-152.24,+450.96,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
Pmove=(-376.27,+80.46,+600.00,-179.60,+0.41,+0.00,+0.00,+0.00)(7,0)
PEscapePosition=(+247.61,-0.38,+580.00,-180.00,+0.00,-179.99)(7,0)
PEscapePosition_2=(-131.77,+177.64,+579.99,-180.00,+0.00,-53.32)(7,0)
PEscapePosition_3=(-178.95,+226.16,+579.83,-180.00,-0.02,-53.33)(7,0)
PEscapePosition_4=(-221.18,+0.03,+579.99,-180.00,+0.00,+0.10)(7,0)
PInitialPosition=(+300.00,+0.00,+440.00,-180.00,+0.00,-180.00)(7,0)
PProductOnJigGet=(-246.30,+1.03,+430.36,-57.93,+88.69,+121.39)(6,1)
PProductOnJigGet_1=(-246.30,+1.03,+460.00,-57.93,+88.69,+121.39)(6,1)
PProductOnJigGet_2=(-190.37,+1.03,+560.00,-57.98,+88.69,+121.39)(6,0)
PProductOnJigGet_3=(-133.01,+133.47,+580.00,-173.13,+89.99,-38.35)(6,0)
PProductOnJigGet_4=(-164.65,+0.00,+671.53,-166.66,+90.00,+13.34)(7,0)
PProductOnJigGet_5=(-224.30,+0.01,+604.77,+180.00,+0.00,+0.00)(7,0)
PProductOnJigGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnJigSet=(-246.30,+1.03,+430.36,-57.93,+88.69,+121.39)(6,0)
PProductOnJigSet_1=(-246.30,+1.03,+460.00,-57.93,+88.69,+121.39)(6,1)
PProductOnJigSet_2=(-190.37,-0.40,+580.00,-48.95,+88.69,+130.94)(6,1)
PProductOnJigSet_3=(-136.17,+133.06,+580.00,-175.95,+89.99,-39.47)(6,0)
PProductOnJigSet_4=(+163.76,+97.11,+580.00,-176.54,+89.98,-145.04)(6,0)
PProductOnPltGet=(+548.95,-100.11,+250.16,-179.40,+0.17,-178.85)(7,0)
PProductOnPltGet_1=(+548.95,-100.11,+290.00,-179.40,+0.17,-178.85)(7,0)
PProductOnPltGet_2=(+548.95,-100.11,+400.00,-179.40,+0.17,-178.85)(7,0)
PProductOnPltSet=(+548.95,-100.11,+249.98,-179.40,+0.17,-178.85)(7,0)
PProductOnPltSet_1=(+548.95,-100.11,+290.00,-179.40,+0.17,-178.85)(7,0)
PProductOnPltSet_2=(+548.95,-100.11,+400.00,+179.50,+0.17,-178.85)(7,0)
PProductOnPltSet_3=(+133.46,+133.02,+580.00,-173.90,+89.99,-129.11)(6,0)
PScrewHeatSink1=(-376.25,+80.46,+568.90,-179.60,+0.40,+0.00)(7,0)
PScrewHeatSink1_0=(-376.25,+80.46,+577.03,-179.60,+0.40,+0.00)(7,0)
PScrewHeatSink1_1=(-376.25,+80.46,+620.00,-179.60,+0.40,+0.00)(7,0)
PScrewHeatSink2=(-376.09,+106.95,+568.90,-179.60,+0.40,+0.00)(7,0)
PScrewHeatSink2_0=(-376.09,+106.95,+577.01,-179.60,+0.40,+0.00)(7,0)
PScrewHeatSink2_1=(-376.09,+106.95,+620.00,-179.60,+0.40,+0.00)(7,0)
PScrewPlateL=(-284.78,+112.54,+536.90,-180.00,+0.00,-90.00)(7,0)
PScrewPlateL1=(-315.02,+23.87,+537.23,+179.56,+1.14,-90.01)(7,0)
PScrewPlateL1_0=(-315.02,+23.87,+542.86,+179.56,+1.14,-90.01)(7,0)
PScrewPlateL1_1=(-315.02,+23.87,+570.00,+179.56,+1.14,-90.01)(7,0)
PScrewPlateL2=(-315.09,+99.12,+537.23,+179.56,+1.15,-90.00)(7,0)
PScrewPlateL2_0=(-315.09,+99.12,+542.86,+179.52,+1.15,-90.00)(7,0)
PScrewPlateL2_1=(-315.09,+99.12,+570.00,+179.52,+1.15,-90.00)(7,0)
PScrewPlateL_0=(-284.78,+112.54,+543.53,-180.00,+0.00,-90.00)(7,0)
PScrewPlateL_1=(-284.78,+112.54,+570.00,-180.00,+0.00,-90.00)(7,0)
PScrewPlateR=(-286.12,-114.33,+536.39,-180.00,+0.00,-90.00)(7,1)
PScrewPlateR1=(-317.51,-31.47,+538.71,-179.73,+1.08,-89.97)(7,1)
PScrewPlateR1_0=(-317.51,-31.47,+544.46,-179.73,+1.08,-89.97)(7,1)
PScrewPlateR1_1=(-317.51,-31.47,+570.00,-179.73,+1.08,-89.97)(7,1)
PScrewPlateR2=(-318.59,-106.60,+538.71,-179.73,+1.08,-89.97)(7,1)
PScrewPlateR2_0=(-318.59,-106.60,+544.46,-179.73,+1.08,-89.97)(7,1)
PScrewPlateR2_1=(-318.59,-106.60,+570.00,-179.73,+1.08,-89.97)(7,1)
PScrewPlateR_0=(-286.12,-114.33,+543.60,-180.00,+0.00,-90.00)(7,1)
PScrewPlateR_1=(-286.12,-114.33,+570.00,-180.00,+0.00,-90.00)(7,1)
PScrewSupplyHS=(-283.07,-241.91,+402.30,+179.99,+0.00,-58.70)(7,1)
PScrewSupplyHS_1=(-283.07,-241.91,+422.55,+179.99,+0.00,-58.70)(7,1)
PScrewSupplyHS_2=(-147.31,-147.28,+610.00,-180.00,+0.00,-45.01)(7,1)
PScrewSupplyHS_3=(-235.18,-0.02,+609.98,+180.00,+0.00,+0.00)(7,1)
PScrewSupplyHS_4=(-271.46,-216.69,+422.80,-180.00,+0.00,-90.00)(7,1)
PScrewSupplyHS_5=(-164.65,+0.00,+671.53,-166.66,+90.00,+13.34)(7,0)
PScrewSupplyHS_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PScrewSupplyPlate=(-134.50,+200.27,+451.67,-180.00,+0.01,+170.74)(7,0)
PScrewSupplyPlate_1=(-134.50,+200.27,+470.00,-180.00,+0.01,+170.74)(7,0)
PScrewSupplyPlate_2=(-134.50,+200.27,+570.00,-180.00,+0.01,+170.74)(7,0)
PScrewSupplyPlate_3=(-132.53,+160.72,+570.00,-179.99,+0.00,-140.43)(7,0)
PScrewSupplyPlate_4=(-208.31,+0.23,+610.00,-180.00,+0.00,-90.00)(7,0)
PScrewSupplyPlate_5=(-113.97,+113.99,+696.72,-0.32,+89.24,+134.68)(7,0)
PScrewSupplyPlate_6=(-161.19,+0.02,+696.72,-0.32,+89.24,+179.67)(7,0)
PScrewSupplyPlate_7=(-50.58,+239.35,+544.69,-180.00,+0.01,+170.74)(7,0)
PScrewSupplyPlatel_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PTicketRead=(+601.26,-152.24,+374.96,-180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+601.26,-152.24,+450.96,-180.00,+0.00,+90.00)(7,0)
PTorqueCheck=(+148.84,-273.37,+340.50,-180.00,-0.01,+110.02)(7,0)
PTorqueCheck_1=(+148.84,-273.37,+360.50,-180.00,-0.01,+110.02)(7,0)
JActive=(+168.00,+8.64,+85.03,-0.31,+85.85,-11.98,+0.00,+0.00)
Jmove=(+168.00,-44.94,+112.38,+0.00,+76.27,+0.00,+0.00,+0.00)
JTaihi=(+0.00,-44.94,+112.38,+0.00,+76.27,+0.00)
