1 ' ===================================
2 '
3 '  21054001 STEP5 Assy6�v���O����
4 '
5 ' �쐬�ҁFM.Hayakawa
6 ' �쐬���F2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1���痬�p
8 '
9 ' Ver 0.3 2021.12.17 �摜�����֐�ISInspection��ISInspectionSingle�A�摜�����ǉ� file:210542003
10 ' ===================================
11 '===== <Insight�萔> =====
12 '===== <Insight�ϐ���`> =====
13 Dim PInspPosition(30)               '�摜����Function���n���p�ʒu�ϐ�
14 Dim MInspGroup%(30)                 '�摜����Function���n���p�ϐ�
15 Def Inte MIN_IS_Ready               '�y����IO�zInsight����OK
16 Def Inte MIN_IS_JobLoadOK           '�y����IO�zInsight�W���u���[�h����I��
17 Def Inte MIN_IS_JobLoadNG           '�y����IO�zInsight�W���u���[�h�ُ�I��
18 Def Inte MIN_IS_InspGSetOK          '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
19 Def Inte MIN_IS_InspGSetNG          '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
20 Def Inte MIN_IS_InspOK              '�y����IO�zInsight����OK
21 Def Inte MIN_IS_InspNG              '�y����IO�zInsight����NG
22 Def Inte MIN_IS_InspErr             '�y����IO�zInsight�����ُ�I��
23 Def Inte MIN_IS_InspCapDone         '�y����IO�zInsight�����摜�捞����
24 '
25 Def Inte MIN_IS_ErrNum              '�y����IO�zInsight�����G���[�ԍ��擾�J�n�A�h���X(16bit)
26 'Output Signal
27 Def Inte MOUT_IS_JobLoadReq         '�y�o��IO�zInsight JOB���[�h�v��
28 Def Inte MOUT_IS_InspGSetReq        '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
29 Def Inte MOUT_IS_Insp               '�y�o��IO�zInsight �������s�v��
30 '
31 Def Inte MOUT_IS_JobNum             '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
32 Def Inte MOUT_IS_InspGNum           '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
33 '
34 Def Inte MOUT_InspErrNum            '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
35 Def Inte MOUT_InspNGStepNum         '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
36 Def Inte MOUT_OKNG                 '
37 '��Ɨp�ϐ�
38 Def Inte MInspErrNum                '�������s�G���[�ԍ�
39 Def Inte MInspNGStepNum             '�������sNGStep�ԍ�
40 Def Inte MRtn                       'Function�߂�l�擾�p
41 Def Inte MRtn2                      'Function�߂�l�擾�p
42 Def Inte MRet3                      'Function�߂�l�擾�p
43 Def Inte MGRtn                      'Function�߂�l�擾�p �l�W�����@
44 Def Inte MInspErrNumSub             '�������s�G���[�ԍ�sub�@20190820�ǉ�
45 Def Inte MovrdA                     '�l�W����Ovrd �ϗp
46 Def Float MSpdA                     '�l�W����Spd�@�ϗp
47 Def Pos PTemp                       '�l�W���ߏ��ʒu�v�Z�p
48 '===== <Insight�ϐ��ݒ�> =====
49 MIN_IS_Ready%        =   11380      '�y����IO�zInsight����OK
50 MIN_IS_JobLoadOK%    =   11381      '�y����IO�zInsight�W���u���[�h����I��
51 MIN_IS_JobLoadNG%    =   11382      '�y����IO�zInsight�W���u���[�h�ُ�I��
52 MIN_IS_InspGSetOK%   =   11383      '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
53 MIN_IS_InspGSetNG%   =   11384      '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
54 MIN_IS_InspOK%       =   11385      '�y����IO�zInsight����OK
55 MIN_IS_InspNG%       =   11386      '�y����IO�zInsight����NG
56 MIN_IS_InspErr%      =   11387      '�y����IO�zInsight�����ُ�I��
57 MIN_IS_InspCapDone%  =   11388      '�y����IO�zInsight�����摜�捞����
58 MIN_IS_ErrNum%       =   11408      '�y����IO�zInsight�����G���[�ԍ��J�n�A�h���X(16bit)
59 'Output Signal
60 MOUT_IS_JobLoadReq%  =   12370      '�y�o��IO�zInsight JOB���[�h�v��
61 MOUT_IS_InspGSetReq% =   12371      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
62 MOUT_IS_Insp%        =   12372      '�y�o��IO�zInsight �������s�v��
63 MOUT_IS_JobNum%      =   12384      '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
64 MOUT_IS_InspGNum%    =   12400      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
65 MOUT_InspErrNum%     =   12416      '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
66 MOUT_InspNGStepNum%  =   12432      '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
67 '===== <�d�h���ϐ���`> =====
68 X20_Driver=11248                    '�d�h���X�e�C�^�X1�@Driver Status 1
69 X21_Driver=11249 '�d�h���X�e�C�^�X2  Driver Status 2
70 X22_Driver=11250 '�d�h���X�e�C�^�X3  Driver Status 3
71 X23_Driver=11251 '�d�h���X�e�C�^�X4  Driver Status 4
72 X24_Driver=11252 '�d�h���G���[���b�Z�[�W1 Driver Error E1
73 X25_Driver=11253 '�d�h���G���[���b�Z�[�W2 Driver Error E2
74 X26_Driver=11254 '�d�h���G���[���b�Z�[�W3 Driver Error E3
75 X27_Driver=11255 '�d�h���G���[���b�Z�[�W4 Driver Error E4
76 X28_Driver=11256 '�d�h���g�[�^���G���[�V�O�i�� Total Error
77 X29_Driver=11257 '�d�h���I���V�O�i�� Comlete signal
78 X2A_Driver=11258 '�d�h���G���[���b�Z�[�W5 Driver Error E5
79 '11584   'toRB�g���N�h���C�o-COMP_ERR���M
80 Y60_Driver=12240 '�d�h�������v��� CCW
81 Y61_Driver=12241 '�d�h�����v��� CW
82 Y62_Driver=12242 '�o���N�Z�b�e�B���O BANK C1
83 Y63_Driver=12243 '�o���N�Z�b�e�B���O BANK C2
84 Y64_Driver=12244 '�o���N�Z�b�e�B���O BANK C3
85 Y65_Driver=12245 '�v���O�����Z�b�e�B���O PRG SET F1
86 Y66_Driver=12246 '�v���O�����Z�b�e�B���O PRG SET F2
87 Y67_Driver=12247 '�v���O�����Z�b�e�B���O PRG SET F3
88 X34_ScrewReady1=11259 '�˂�����1�@Read
89 '===== <�d�h���萔> =====
90 Dim PScrewPos(10)       '�l�W���ߗpFunction�����ϐ�
91 Dim PGetScrewPos(10)    '�˂������@����˂��𓾂�Function�����ϐ�
92 Dim PEscapePosi(10)
93 MLoopCnt% = 0'
94 '===== <���{�b�g�萔> =====
95 '===== <���{�b�g�ϐ���`> =====
96 MRBTOpeGroupNo = 0      '���{�b�g����ԍ�������
97 MCommentD1001 = 0
98 MCommentD1002 = 0
99 MCommentD1003 = 0
100 MScreenNo = 0
101 '
102 MCommentTSU = 0
103 MCommentTSD = 0
104 '�E�B���h��ʔԍ��ݒ�
105 MWindReSet = 0
106 MWindInfoScr = 5
107 MWindErrScr = 10
108 MWindErrScr2 = 11
109 MWindErrScr3 = 13
110 MWindErrScr17 = 17
111 MWindErrScr18 = 18
112 MWindCmmnScr = 20
113 MWindJigRelase19049 = 60
114 MWindJigRelase19050 = 61
115 MWindJigRelase19051 = 62
116 '
117 MClear% = 0        'KEY_�̃N���A
118 MAbout% = 1        'KEY_��~
119 MNext% = 2         'KEY_���̃X�e�b�v�ֈڍs
120 MContinue% = 3     'KEY_�p�� �ēx����������s��
121 '
122 Def Inte MNgProcess
123 MNgProcess% = 5      'KEY_NG
124 '
125 MAssyOK% = 6       '�g������
126 MPass% = 7         '�H���p�X
127 MPiasNG% = 8       'Pias�m�F������NG
128 MIrregular% = 10   '��O�������s�p
129 '
130 '�������pKEY�ԍ�   '
131 MRobotInit1% = 11  '�����ʒu�p
132 MRobotInit2% = 12  '�����ʒu�p
133 MRobotInit3% = 13  '�����ʒu�p
134 MRobotInit4% = 14  '�����ʒu�p
135 '
136 MIN_INIT1REQUEST% = 11568 'toRBT_���{�b�g�����ʒu1�v��
137 MIN_INIT2REQUEST% = 11569 'toRBT_���{�b�g�����ʒu2�v��
138 MIN_INIT3REQUEST% = 11570 'toRBT_���{�b�g�����ʒu3�v��
139 MIN_INIT4REQUEST% = 11571 'toRBT_���{�b�g�����ʒu4�v��
140 '
141 MOUT_INIT1RECIVE% = 12560 'toPLC_���{�b�g�����ʒu1��M
142 MOUT_INIT2RECIVE% = 12561 'toPLC_���{�b�g�����ʒu2��M
143 MOUT_INIT3RECIVE% = 12562 'toPLC_���{�b�g�����ʒu3��M
144 MOUT_INIT4RECIVE% = 12563 'toPLC_���{�b�g�����ʒu4��M
145 '
146 MOK% = 1               '�e����p
147 MNG% = 0               '�e����p
148 MTIMEOUT% = -1         '�e����p
149 MJudge% = 0            '������i�[�p
150 '
151 MRECIVETIME& = 0
152 MSETTIMEOUT10& = 10000&                '10�b�ݒ�
153 MSETTIMEOUT03& = 3000&                 '3�b�ݒ�
154 MSETTIMEOUT01& = 1000&                 '1�b�ݒ�
155 MSETTIMEOUT05& = 5000&                 '5�b�ݒ�
156 MSETTIMEOUT009& = 900&                 '0.9�b�ݒ�
157 MSETTIMEOUT008& = 800&                 '0.8�b�ݒ�
158 MSETTIMEOUT007& = 700&                 '0.7�b�ݒ�
159 MSETTIMEOUT006& = 600&                 '0.6�b�ݒ�
160 MSETTIMEOUT005& = 500&                 '0.5�b�ݒ�
161 MSETTIMEOUT004& = 400&                 '0.4�b�ݒ�
162 MSETTIMEOUT003& = 300&                 '0.3�b�ݒ�
163 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
164 MIN_PIAS_ComOK% = 11552                'PC�ʐMOK
165 MIN_PIAS_ComTimeOut% = 11576           'PC�ʐM�m�F�^�C���A�E�g
166 MIN_PIAS_ComNG% = 11553                'PC�ʐMNG
167 MOUT_PIAS_ComCheck% = 12544            'PC�ʐM�m�F�v��
168 MOUT_PIAS_Missing_Process% = 12546     '�H�������m�F�v��
169 MIN_PIAS_ModelTypeNG% = 11554          '���f���d��NG
170 MIN_PIAS_ProcessHistryNG% = 11555      '�O�H������NG
171 MIN_PIAS_ProcessHistryOK% = 11556      '�O�H������OK
172 MIN_PIAS_ProcessHistryErr% = 11557     '�H�����������G���[
173 MIN_PIAS_MyProcessComp% = 11573        '���H����������
174 MIN_PIAS_ProcessHistryTimeOut% = 11578 '�H�������^�C���A�E�g
175 MOUT_OKNG% = 12226                     'PLC OUT ��OK=1, NG=0 �o��
176 '
177 MOUT_PiasPCBNumberCheck = 12557        '��ԍ��ƍ�
178 MIN_PiasPCBNumberOK% = 11566          '��ԍ�OK
179 MIN_PiasPCBNumberNG% = 11565          '��ԍ�NG
180 MIN_PiasPCBNumberErr% = 11567         '��ԍ������G���[
181 '
182 MOUT_PiasAssyResultOK% = 12549    '�g��OK
183 MOUT_PiasAssyResultNG% = 12550    '�g��NG
184 MOUT_PiasAssyResultWr% = 12548    '�H��������������
185 '
186 MIN_PiasProcessNG% = 11559        '�H����������NG
187 MIN_PiasProcessOtherErr% = 11560  '�H�����������G���[(�Ȃ񂩂̃g���u��)
188 MIN_PiasProcessOK% = 11558        '�H����������OK
189 '
190 MIN_Insight_Use% = 11374               '�摜�m�FON
191 MIN_TorqueCheck% = 11348               '�g���N�`�F�b�N
192 '
193 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT���쌠
194 MOUT_RED_LIGHT% = 12356            'PATLIGHT �� �_��
195 MOUT_RED_FLASH% = 12357            'PATLIGHT �� �_��
196 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT �� �_��
197 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT �� �_��
198 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT �� �_��
199 MOUT_GREEN_FLASH% = 12361          'PATLIGHT �� �_��
200 '
201 MOUT_ST_DATETIME% = 12551          '�g���J�n���t����
202 MOUT_ED_DATETIME% = 12552          '�g���I�����t����
203 '
204 MOUT_TORQUE_CHECK% = 12367         'PLC�փg���N�`�F�b�N���𑗐M
205 '
206 MIN_ASSY_CANCEL% = 11366           '�g�����s�����̃t���O
207 '
208 MLoopFlg% = 0                      'KEY���͌��OK or NG���e
209 MopeNo% = 0
210 MRtn% = 0
211 MRet = 0
212 MRet3% = 0
213 '
214 Def Inte MInputQty          '������ ���Z�ϐ�
215 Def Inte MAssyOkQty         '�g���n�j�� ���Z�ϐ�
216 Def Inte MAssyNgQty         '�g���m�f�� ���Z�ϐ�(���g�p)
217 Def Inte MSuctionErrQty     '�z���G���[�� ���Z�ϐ� 2022/04/27 �n��
218 Def Inte nAssyOkQty         '���g�p
219 Def Inte MScrewNo
220 Def Inte MReTry
221 '===== <IO�ϐ���`> =====
222 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
223 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
224 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
225 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
226 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
227 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
228 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
229 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
230 '
231 Def Inte Y6A_VV1            ' �A�[����[�@�l�W�z���o���u
232 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
233 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
234 '
235 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
236 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
237 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
238 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
239 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
240 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
241 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
242 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
243 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
244 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
245 '
246 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
247 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
248 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
249 '
250 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
251 '
252 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
253 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
254 '
255 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
256 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
257 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
258 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
259 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
260 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
261 '
262 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
263 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
264 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
265 '
266 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
267 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
268 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
269 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
270 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
271 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
272 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
273 Y6A_VV1%    =  12250    ' �A�[����[�@�l�W�z���o���u
274 Y6B_VB1%    =  12251    '�A�[����[�@�z���j��o���u
275 MOUT_VB1%   =  12251    ' �A�[����[�@�l�W�z���j��o���u
276 '
277 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
278 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
279 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
280 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
281 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
282 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
283 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
284 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
285 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
286 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
287 '
288 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
289 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
290 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
291 '
292 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
293 '
294 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
295 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
296 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
297 '
298 '����
299 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
300 Def Inte MOn                            '�o��=1
301 Def Inte MOff                           '�o��=0
302 '
303 '�˂����ߑ��u_�o�̓A�h���X
304 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
305 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
306 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
307 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
308 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
309 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
310 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
311 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
312 '�˂����ߑ��u_���̓A�h���X
313 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
314 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
315 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
316 Def Inte MIN_ScwT_Case1                 '����1��~����M
317 Def Inte MIN_ScwT_Case2                 '����2��~����M
318 Def Inte MIN_ScwT_Case3                 '����3��~����M
319 Def Inte MIN_ScwT_Case4                 '����4��~����M
320 Def Inte MIN_ScwT_Case5                 '����5��~����M
321 '
322 Dim MScwT_Case1%(2)               '����1��~�ϐ�
323 Dim MScwT_Case2%(2)               '����2��~�ϐ�
324 Dim MScwT_Case3%(2)               '����3��~�ϐ�
325 Dim MScwT_Case4%(2)               '����4��~�ϐ�
326 Dim MScwT_Case5%(2)               '����5��~�ϐ�
327 '
328 '����
329 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
330 MOn% = 1                                 '�o�� = 1
331 MOff% = 0                                '�o�� = 0
332 '
333 '�˂����ߋ@_�A�h���X�ݒ�
334 MOUT_ScwT_ComChk% = 12832               '�ʐM�m�F���M
335 MOUT_ScwT_ST% = 12865                   '�˂����ߊJ�n�𑗐M
336 MOUT_ScwT_ReSTOK% = 12866               '�ĊJ�n��M�𑗐M
337 MOUT_ScwT_FinOK% = 12868                '�˂����ߊ�����M�𑗐M
338 MOUT_ScwT_Case1OK% = 12858              '����1��~��M�𑗐M
339 MOUT_ScwT_Case2OK% = 12859              '����2��~��M�𑗐M
340 MOUT_ScwT_Case3OK% = 12860              '����3��~��M�𑗐M
341 MOUT_ScwT_Case4OK% = 12861              '����4��~��M�𑗐M
342 MOUT_ScwT_Case5OK% = 12862              '����5��~��M�𑗐M
343 '
344 MIN_ScwT_comOK% = 11840                 '�˂����ߑ��u����ԐM
345 MIN_ScwT_STRec% = 11873                 '�˂����ߊJ�n����M
346 MIN_ScwT_ReST% = 11874                  '�ĊJ�n����M
347 MIN_ScwT_Fin% = 11876                   '�˂����ߊ�������M
348 MIN_ScwT_Case1% = 11866                 '����1��~�ҋ@����M
349 MIN_ScwT_Case2% = 11867                 '����2��~�ҋ@����M
350 MIN_ScwT_Case3% = 11868                 '����3��~�ҋ@����M
351 MIN_ScwT_Case4% = 11869                 '����4��~�ҋ@����M
352 MIN_ScwT_Case5% = 11870                 '����5��~�ҋ@����M
353 '
354 MScwT_Case1%(1) = MIN_ScwT_Case1%
355 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
356 MScwT_Case2%(1) = MIN_ScwT_Case2%
357 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
358 MScwT_Case3%(1) = MIN_ScwT_Case3%
359 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
360 MScwT_Case4%(1) = MIN_ScwT_Case4%
361 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
362 MScwT_Case5%(1) = MIN_ScwT_Case5%
363 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
364 '
365 '�ݒ� InitialZoneB�Ŏg�p����ϐ�
366 Def Pos PActive       '�������W�n �ʒu�ϐ� ���݈ʒu
367 Def Pos Pmove         '�������W�n �ʒu�ϐ� �ړ���
368 Def Jnt JActive       '�֐ߍ��W�n �ʒu�ϐ� ���݈ʒu
369 Def Jnt Jmove         '�֐ߍ��W�n �ʒu�ϐ� �ړ���
370 Def Jnt JTaihi        '�֐ߍ��W�n �ʒu�ϐ� �ޔ��|�W�V���� �e�B�[�`���O�Őݒ�
371 Def Inte MRecoveryPass         '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s
372 Def Inte MStandby              '�ҋ@�ʒu�m�F�t���O
373 Def Inte MRecoveryChuckOpen    '�`���b�N����t���O�i���A����O�j���͂ݑ΍�
374 '�����Ӂ������ʒu��ύX�������ɂ́A�ύX���K�v�I
375 '
376 '
377 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
378 Function M% fnAssyStart
379     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
380 ' PIAS�`�P�b�g�Ǎ��ݍH�������m�F
381     M_20# = MClear%                       '������
382     If M_22# = MIrregular% Then GoTo *IRREGULAR     'Assy������DVD���J��c�����Ă���ꍇ
383     '�J�n�ʒu���C�j�V�����|�W�V�����������ꍇ(�n���h��������������`�F�b�N)
384     *RE_START
385     PTemp = P_Curr
386     MRtn = 0
387     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
388         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
389             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
390                 MRtn = 1
391             EndIf
392         EndIf
393     EndIf
394     '�C�j�V�����|�W�V�����ɂ����ꍇ�Z���T�[�`�F�b�N
395     If MRtn = 1 Then
396         Ovrd 20
397         Accel 100 , 20
398         Mvs PHandChange                             '�n���h�����ʒu�Ɉړ�
399         Ovrd 100
400         Accel 100 , 100
401         MRtn = frInCheck(11264,0,MSETTIMEOUT03&)    '�Z���T�[�`�F�b�N(���������)
402         Mvs PInitialPosition                        '�C�j�V�����|�W�V�����Ɉړ�
403     Else
404         MRtn = 1
405     EndIf
406     If MRtn = 1 Then GoTo *AssyStart
407     fErrorProcess(11,286,287,0)                        '�n���h�����p�������
408     If M_20# = MNext% Then M_20# = MClear%
409     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
410     If M_20# = MNgProcess% Then GoTo *RE_START
411     If M_20# = MContinue% Then GoTo *RE_START
412 '
413     *AssyStart
414 '
415 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
416 '        MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
417 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
418 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
419 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' ���j���[�֖߂�
420 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NG���H�������ɏ����ݎ��̍H����
421 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NG���H�������ɏ����ݎ��̍H����
422 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' ����NG, �H������
423 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK�ȊO�͑g���I��
424 '    EndIf
425 ' �l�W���ߋ@�e�X�g�p ----------
426 '    'Mret% = fScewTcomChk()
427 '    '�˂����ߊJ�n
428     MRtn2 = fScewTStart()
429 '    '
430 '    '���W�ړ�
431 '    '
432 '    '����xx��~
433 '    fScewTCaseStop(MScwT_Case5%)
434 '    '
435 '    '�x�[�X���j�b�gKEY
436 '    Wait M_In(MTEST_KEY%) = MOn%
437 '    '
438 '    '�ĊJ�n
439 '    fScewTReStart()
440 '    '
441 '    '���W�ړ�
442 '    '
443 '    '�˂����ߊ���
444 '    Mret% = fScewTFinish()
445 ' �l�W���߃e�X�g�I��
446 ' PIAS�e�X�g -----------
447 '    MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
448 '    MRet% = fnPiasWrite(MNG%)
449  '   MRet% = fnPCBNumberCheck()     '�f�o�b�N�p�ɃR�����g�A�E�g(9/17����)
450 ' PIAS�e�X�g�I�� -------
451 '�g�ݗ��ĊJ�n(���g��9/10����)
452 '�v���O�������_
453 Ovrd 100
454 If MRtn2 = 0 Then GoTo *INITIAL_CHECK
455     fErrorProcess(11,329,201,0)
456     If M_20# = MNext% Then GoTo *AssyStart
457     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
458     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
459     If M_20# = MContinue% Then GoTo *AssyStart
460 '�n���h��DVD���J,�u���P�b�g��������
461 '
462 *INITIAL_CHECK
463 '
464 If M_In(11264) = 0 And M_In(11267) = 0 And M_In(11270) = 0 Then GoTo *CompInitial1  'DVD���J,�u���P�b�g��������
465 fErrorProcess(11,253,287,0)                         '284��287�ɕύX6/2����
466 If M_20# = MNext% Then M_20# = MClear%
467 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
468 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
469 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
470 *CompInitial1
471 '
472 '�n���h���C�j�V�����ɖ߂�
473 If M_In(11266) = 1 Then     'DVD�`���b�N���o
474     M_Out(12256) = 0        'DVD�`���b�N��OFF
475     M_Out(12257) = 1        'DVD�`���b�N�JON
476     Break
477 EndIf
478 If M_In(11269) = 1 Then     'F�V�����_�[�o���o
479     M_Out(12258) = 0        'F�V�����_�[�oOFF
480     M_Out(12259) = 1        'F�V�����_�[��ON
481     Break
482 EndIf
483 If M_In(11272) = 1 Then     'R�V�����_�[�o���o
484     M_Out(12260) = 0        'R�V�����_�[�oOFF
485     M_Out(12261) = 1        'R�V�����_�[��ON
486     Break
487 EndIf
488 '
489 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    'DVD�`���b�N�J���o
490 If MRtn = 1 Then GoTo *CompInitial2
491 fErrorProcess(11,270,284,0)
492 If M_20# = MNext% Then M_20# = MClear%
493 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
494 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
495 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
496 *CompInitial2
497 '
498 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)    'F�V�����_�[�ߌ��o
499 If MRtn = 1 Then GoTo *CompInitial3
500 fErrorProcess(11,278,284,0)
501 If M_20# = MNext% Then M_20# = MClear%
502 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
503 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
504 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
505 *CompInitial3
506 '
507 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)    'R�V�����_�[�ߌ��o
508 If MRtn = 1 Then GoTo *CompInitial4
509 fErrorProcess(11,276,284,0)
510 If M_20# = MNext% Then M_20# = MClear%
511 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
512 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
513 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
514 *CompInitial4
515 '
516 '
517 ' 2022/04/11 ���S�����֏����ǉ� �n��
518 ' PInitialPosition �ݐ� MStandby=1
519 ' PMechaOnJigGet_3 �ݐ� MStandby=2
520 '
521 MStandby = 0    '�ҋ@�ʒu�t���O��������
522 PTemp = P_Curr
523 If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
524     If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
525         If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
526             MStandby = 1
527         EndIf
528     EndIf
529 EndIf
530 If (PTemp.X <= PMechaOnJigGet_3.X + 1.0) And (PTemp.X >= PMechaOnJigGet_3.X - 1.0) Then
531     If ((PTemp.Y <= PMechaOnJigGet_3.Y + 1.0) And (PTemp.Y >= PMechaOnJigGet_3.Y - 1.0)) Then
532         If ((PTemp.Z <= PMechaOnJigGet_3.Z + 1.0) And (PTemp.Z >= PMechaOnJigGet_3.Z - 1.0)) Then
533             MStandby = 2
534         EndIf
535     EndIf
536 EndIf
537 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
538     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
539         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
540             MStandby = 3
541         EndIf
542     EndIf
543 EndIf
544 If MStandby <> 0 Then GoTo *PositionOK
545 fErrorProcess(11,230,281,0)          '�����ʒu�ɂ��Ȃ����̓G���[�ɂ���
546 If M_20# = MNext% Then GoTo *ASSY_ERROR_END
547 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
548 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
549 If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
550 *PositionOK
551 '
552 '
553 'DVD����PASS�v���O��������ʏ�v���O�����ɐؑւ������̑΍� 2022.05.13 �n��
554 PTemp = P_Curr
555 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
556     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
557         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
558             Ovrd 50
559             Mov PInitialPosition
560             Ovrd 100
561         EndIf
562     EndIf
563 EndIf
564 '
565 '
566 *ReCheck1
567     M_Out(12912) = 1                  'Ver 0.4 �ǉ��@���u����t���O����(�H��6�D��̂��ߍH��5�̃t���O�Ď��̑O�ɏo��)
568     Mov PMechaOnJigGet_3    '�^�N�g�Z�k�̂��ߏ����ʒu�ύX
569     'Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�����ʒu�ړ�2/9����)
570     MRtn = fTimeOutJudge(11920,0)    'BaseUnit5���u����t���O�m�F(�^�C���A�E�g������ǉ�22/09/29����)
571     If MRtn = 0 Then GoTo *ASSY_ERROR_END
572     If MRtn = 2 Then GoTo *ReCheck1
573     M_Out(12912) = 1                  '���u����t���O����(�O�̂��ߒǉ�2/9����)
574 '
575 'Ver 0.4 �ǉ�--------------------
576 If M_In(11920) = 1 Then GoTo *CompInitial4         '�H��6�����쒆�̏ꍇ11920 = 1 ���[�v
577 'Ver 0.4 �����܂�----------------           '�H��6�D��̂���12912=0�ɂ��Ȃ�
578 '
579 'Mov PInitialPosition   '1/20�R�����g�A�E�g(����)
580 '
581 '���u������_�ֈړ�
582 Mov PMechaOnJigGet_2         '���u������_
583 '
584 '���u���䂩��DVD���J���󂯎��
585 'Wait M_In(12914) = 1              '����������M
586 'Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�ǉ���������10/1����)
587 '
588     MRtn2 = 0
589 *RE_JIG_GET
590     M_20# = MClear%
591 '
592     If M_In(11278) = 1 Then          '�����̊m�F(CW�[�Z���T�[)(�ǉ������܂�10/1����)
593         Mov PMechaOnJigGet1_1    '���u������
594         Ovrd 25
595         Mvs PMechaOnJigGet1      'DVD���J�󂯎��ʒu
596         M_Out(12257) = 0         'DVD���J�`���b�N�JOFF
597         M_Out(12256) = 1         'DVD���J�`���b�N��ON
598 '        Wait M_In(11266) = 1     'DVD���J�`���b�N���o
599         MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
600         If MRtn = 0 And MRtn2 = 0 Then      'DVD���J��c���ł��Ȃ������ꍇ�`���b�N���J��(6/2����)
601             M_Out(12256) = 0         'DVD���J�`���b�N��OFF
602             M_Out(12257) = 1         'DVD���J�`���b�N�JON
603         EndIf
604         Mvs PMechaOnJigGet1_1    '���u������
605         Ovrd 100
606         Break
607     ElseIf M_In(11277) = 1 Then       '�����̊m�F(CCW�[�Z���T�[)(�ǉ���������10/1����)
608         Mov PMechaOnJigGet2_1    '���u������
609         Ovrd 25
610         Mvs PMechaOnJigGet2      'DVD���J�󂯎��ʒu
611         M_Out(12257) = 0         'DVD���J�`���b�N�JOFF
612         M_Out(12256) = 1         'DVD���J�`���b�N��ON
613 '        Wait M_In(11266) = 1     'DVD���J�`���b�N���o
614         MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
615         If MRtn = 0 And MRtn2 = 0 Then  'DVD���J��c���ł��Ȃ������ꍇ�`���b�N���J��(6/2����)
616             M_Out(12256) = 0         'DVD���J�`���b�N��OFF
617             M_Out(12257) = 1         'DVD���J�`���b�N�JON
618         EndIf
619         Mvs PMechaOnJigGet2_1    '���u������
620         Ovrd 100
621         Break
622     EndIf
623 '
624     If MRtn = 1 Or MRtn2 = 1 Then GoTo *CompMechaGet1    'DVD���J�`���b�N�G���[����(���ւ������ꂽ�����G���[�\���Ȃ��Ő�ɐi�ނ悤�ɕύX(6/2����))
625 '    PTemp = P_Curr              '�����ʒu�̕ύX1/12����
626 '    Mvs PTemp , -150
627     Mov PMechaOnJigGet_2
628     fErrorProcess(11,269,294,0)  '279,284��269,294�ɕύX6/2����
629 '    PTemp = P_Curr             '�����ʒu�̕ύX1/12����
630 '    Mvs PTemp , -150
631 '    Mov PMechaOnJigGet_2
632     If M_20# = MNext% Then
633         M_20# = MClear%
634         MRtn2 = 1
635     EndIf
636     If M_20# = MContinue% Then MRtn2 = 0
637     If M_20# = MAbout% Or M_20# = MNgProcess% Then
638         Mov PInitialPosition
639         Break
640     EndIf
641     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
642     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
643     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_JIG_GET
644 *CompMechaGet1
645 '
646     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
647 Mov PMechaOnJigGet_2            '���u������_
648     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
649 '
650 'DVD���J�Z���T�[���o
651 '
652 'M_Out(12912) = 0                  '���u����t���O���(�����ʒu�ύX2/9����)
653 '
654 '
655 'DVD���J���˂����{3�ɒu��
656 Mov PMechaOnRoboSet_2        '�˂����{���_
657 '
658 'Wait M_In(11888) = 1         '�˂����{3��~1�܂őҋ@
659 MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
660 If MRtn = 0 Then Mov PInitialPosition     '"�C�j�V�����ɖ߂铮��"
661 If MRtn = 0 Then GoTo *ASSY_ERROR_END
662 '
663 Mov PMechaOnRoboSet_1        '�˂����{���
664 Ovrd 25
665 Mvs PMechaOnRoboSet          'DVD���J�u����
666 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~1�`��~2�܂�)
667 '
668 'Wait M_In(11889) = 1         '�˂����{3��~2�܂őҋ@
669 MRtn = fScrewTighenRoboCheck(11889)    '��~��Ԃ���M����
670 'If MRtn = 0 Then
671 '    M_Out(12256) = 0             'DVD���J�`���b�N��OFF
672 '    M_Out(12257) = 1             'DVD���J�`���b�N�JON
673 '    Mvs PMechaOnRoboSet_1
674 '    Mov PMechaOnRoboSet_2
675 '    Mov PInitialPosition
676 'EndIf
677 If MRtn = 0 Then GoTo *ASSY_ERROR_END   '���̏�Œ�~����(�̏�̉\��)
678 '
679 *RE_ROBO_SET_1
680 '
681 M_Out(12256) = 0             'DVD���J�`���b�N��OFF
682 M_Out(12257) = 1             'DVD���J�`���b�N�JON
683 '
684 'Wait M_In(11265) = 1         'DVD���J�`���b�N�J���o
685 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
686 If MRtn = 1 Then GoTo *CompRoboSet1
687 fErrorProcess(11,270,284,0)
688 If M_20# = MNext% Then M_20# = MClear%
689 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
690 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
691 If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
692 *CompRoboSet1
693 '
694 '    ' ���i�����v�����M(�����ʒu�ύX1/20����)
695     M_Out(12787) = 1
696 '
697 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~2�`��~3�܂�)
698 '
699 'Wait M_In(11890) = 1         '�˂����{3��~3�܂őҋ@
700 MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����
701 MScrewRoboNgFlg% = 0
702 If MRtn = 0 Then MScrewRoboNgFlg% = 1
703 '
704 Mvs PMechaOnRoboSet_1        '�˂����{���
705 Ovrd 100
706 Mov PMechaOnRoboSet_2        '�˂����{���_
707 M_Out(12912) = 0                  '���u����t���O���(�����ʒu�ύX2/9����)
708 '
709 '
710 If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition   '�˂����{�Œ�~��NG��������Ă����ꍇ
711 If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END   '
712 '
713 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~3�`��~4�܂�)
714 '
715 ''    ' ���i�����v�����M(�����ʒu�ʒu�ύX1/20����)
716 '    M_Out(12787) = 1
717 '    '    ' ���i���������҂�
718 '    Wait M_In(11810) = 1
719 '
720 'DVD����(R)�����
721 *RE_R_GET_3
722 Mov PBracketRGet_3           '�o�H
723 Mov PBracketRGet_2           '����(R)�󂯎����_
724 M_Out(12261) = 0             '����(R)�V�����_�[��OFF
725 M_Out(12260) = 1             '����(R)�V�����_�[�oON
726 '
727     '    ' ���i���������҂�(�����ύX2/27����)
728 *RE_FEEDER_READY
729 '    Wait M_In(11810) = 1
730     fnAutoScreenComment(513)    '��ԕ\��[���i�����҂�] 2022/04/26 �n��
731     MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '�����҂�
732     If MRtn = 1 Then GoTo *CompFeederReady
733 '   ' ���i�����v���I��
734     M_Out(12787) = 0
735     fErrorProcess(11,289,290,0) '284��290�ɕύX6/2����
736     If M_20# = MNext% Then M_20# = MClear%
737     If M_20# = MAbout% Or M_20# = MNgProcess% Then
738         Mov PBracketRGet_2
739         Mov PBracketRGet_3
740         Mov PBracketRSet_3
741         Mov PInitialPosition1
742     EndIf
743     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
744     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
745     ' ���i�����v��
746     M_Out(12787) = 1
747     If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
748 *CompFeederReady
749 '    ' ���i�����v���I��
750     M_Out(12787) = 0
751 '
752     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
753     Mov PBracketRGet_1           '����(R)�󂯎����
754 '
755 *RE_R_GET_1
756 '
757 If M_20# = MContinue% Then
758     M_Out(12261) = 0             '����(R)�V�����_�[��OFF
759     M_Out(12260) = 1             '����(R)�V�����_�[�oON
760     M_20# = MClear%
761 EndIf
762 '
763 'Wait M_In(11272) = 1         '����(R)�V�����_�[�o�[���o
764 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '����(R)�V�����_�[�o�[���o
765 If MRtn = 1 Then GoTo *CompRGet1
766 fErrorProcess(11,275,284,0)
767 If M_20# = MNext% Then M_20# = MClear%
768 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
769 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
770 If M_20# = MContinue% Then GoTo *RE_R_GET_1
771 *CompRGet1
772 '
773 Ovrd 25
774 '
775 *RE_R_GET_2
776 '
777 Mvs PBracketRGet             '����(R)�󂯎��ʒu
778 '
779 '
780 M_Out(12252) = 0             '�^��OFF�o���uOFF
781 M_Out(12253) = 0             '�^��j��o���uOFF(�O�̂���)
782 M_Out(12251) = 1             '�^��ON�o���uON
783 '
784 'Wait M_In(11270) = 1         '����(R)�z���Z���T�[ON���o
785 MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   '����(R)�z���Z���T�[ON���o
786 If MRtn = 1 Then GoTo *CompRGet2
787 Mvs PBracketRGet_1           '����(R)�󂯎����
788 fErrorProcess(11,279,295,0)  '284��295�ɕύX6/2����
789 If M_20# = MNext% Then M_20# = MClear%
790 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
791 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
792 If M_20# = MContinue% Then GoTo *RE_R_GET_2
793 *CompRGet2
794 '
795 Mvs PBracketRGet_1           '����(R)�󂯎����
796 Ovrd 100
797 Mov PBracketRGet_2           '����(R)�󂯎����_
798 Mov PBracketRGet_3           '�o�H
799 '
800 'DVD����(R)��u��
801 Mov PBracketRSet_3           '���_
802 MRtn = frInCheck(11270,1,MSETTIMEOUT05&)              '������x����(R)�z���Z���T�[ON���o
803 If MRtn = 1 Then GoTo *CompRGet3
804 fErrorProcess(11,279,295,0)  '284��295�ɕύX6/2����
805 If M_20# = MNext% Then M_20# = MClear%
806 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
807 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
808 If M_20# = MContinue% Then GoTo *RE_R_GET_3
809 *CompRGet3
810 '
811 'Wait M_In(11891) = 1         '�˂����{3��~4�܂őҋ@
812 MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
813 If MRtn = 0 Then Mov PInitialPosition
814 If MRtn = 0 Then GoTo *ASSY_ERROR_END
815 '
816 Mov PBracketRSet_2           '����(R)�u���ʒu���_
817 Mvs PBracketRSet_1           '����(R)�u���ʒu���
818 Ovrd 10
819 Mvs PBracketRSet             '����(R)�u���ʒu
820 Dly 0.1
821 '
822 *RE_R_SET_1
823 '
824 M_Out(12251) = 0             '�^��ON�o���uOFF
825 M_Out(12252) = 1             '�^��OFF�o���uON
826 M_Out(12253) = 1             '�^��j��o���uON
827 '
828 MRtn = frInCheck(11270,0,MSETTIMEOUT05&)
829 '
830 Dly 0.2
831 'M_Out(12253) = 0             '�^��j��o���uOFF
832 '
833 If MRtn = 1 Then GoTo *CompRSet1
834 Mvs PBracketRSet_1           '����(R)�u���ʒu���
835 M_Out(12253) = 0             '�^��j��o���uOFF
836 fErrorProcess(11,296,297,0)  '236,284��296,297�ɕύX6/2����
837 If M_20# = MNext% Then M_20# = MClear%
838 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
839 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
840 If M_20# = MContinue% Then GoTo *RE_R_SET_1
841 *CompRSet1
842 '
843 'M_Out(12260) = 0             '����(R)�V�����_�[�oOFF(�߂��ʒu�ύX1/20����)
844 'M_Out(12261) = 1             '����(R)�V�����_�[��ON
845 ''
846 '*RE_R_SET_2
847 ''
848 ''Wait M_In(11271) = 1         '����(R)�V�����_�[�ߒ[���o
849 'MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   '����(R)�V�����_�[�ߒ[���o
850 'If MRtn = 1 Then GoTo *CompRSet2
851 'fErrorProcess(11,276,284,0)
852 'If M_20# = MNext% Then M_20# = MClear%
853 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
854 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
855 'If M_20# = MContinue% Then GoTo *RE_R_SET_2
856 '*CompRSet2
857 '
858 Ovrd 1                       '����������h�~(5/13����)
859 Mvs PBracketRSet , -5        '����������h�~(5/13����)
860 Ovrd 100                     '����������h�~(5/13����)
861 Mvs PBracketRSet_1           '����(R)�u���ʒu���
862 M_Out(12253) = 0             '�^��j��o���uOFF
863 Ovrd 100
864 Mov PBracketRSet_2           '����(R)�u���ʒu���_
865 Mov PBracketRSet_3           '���_
866 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~4�`��~5�܂�)
867 '
868 *RE_R_SET_2
869 '
870 M_Out(12260) = 0             '����(R)�V�����_�[�oOFF(�߂��ʒu�ύX1/20����)
871 M_Out(12261) = 1             '����(R)�V�����_�[��ON
872 '
873 'Wait M_In(11271) = 1         '����(R)�V�����_�[�ߒ[���o
874 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   '����(R)�V�����_�[�ߒ[���o
875 If MRtn = 1 Then GoTo *CompRSet2
876 fErrorProcess(11,276,284,0)
877 If M_20# = MNext% Then M_20# = MClear%
878 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
879 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
880 If M_20# = MContinue% Then GoTo *RE_R_SET_2
881 *CompRSet2
882 '
883 '
884 '�u���ʒu�摜����(�����ʒu�ړ�)
885 'Wait M_In(11892) = 1         '�˂����{3��~5�܂őҋ@(�摜��������F���u���P�b�g�u��)
886 'MRtn = fScrewTighenRoboCheck(11892)    '��~��Ԃ���M����
887 'If MRtn = 0 Then Mov PInitialPosition
888 'If MRtn = 0 Then GoTo *ASSY_ERROR_END
889 '
890 'Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�ǉ���������10/1����)
891 ''
892 'M_Out(12912) = 1                  '���u����t���O����(�Փ˖h�~)
893 ''
894 ''Mov PBracketRCheck_2         '�o�H
895 ''Mov PBracketRCheck           '�����ʒu
896 ''
897 '*RE_R_CHECK
898 ''If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
899 'PInspPosition(1) = PBracketRCheck1
900 'MInspGroup%(1) = 2
901 'PInspPosition(2) = PBracketRCheck2
902 'MInspGroup%(2) = 3
903 'MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
904 'If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
905 'If MRtn = 0 Then
906 '    MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
907 'EndIf
908 'If MRtn = 1 Then GoTo *CompRCheck
909 'fErrorProcess(11,43,46,0)
910 'If M_20# = MNext% Then M_20# = MClear%
911 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
912 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
913 'If M_20# = MContinue% Then GoTo *RE_R_CHECK
914 '*CompRCheck
915 '*SkipCheck1
916 '
917 'M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~5�`��~6�܂�)
918 '
919 'Mov PBracketRCheck
920 '
921 'DVD����(F)�����
922 *RE_F_GET_3
923 'M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~5�`��~6�܂�)(�����ʒu�ύX1/20����)
924 Mov PBracketFGet_3           '���_
925 '
926 M_Out(12912) = 0                  '���u����t���O���(�Փ˖h�~)
927 '
928 Mov PBracketFGet_2           '����(F)�󂯎����_
929 'Mov PBracketFGet_1           '����(F)�󂯎����(�ړ��^�C�~���O�ύX1/20����)
930 '
931 *RE_F_GET_1
932 '
933 M_Out(12259) = 0             '����(F)�V�����_�[��OFF
934 M_Out(12258) = 1             '����(F)�V�����_�[�oON
935 '
936 Mvs PBracketFGet_1           '����(F)�󂯎����(�ړ��^�C�~���O�ύX1/20����)
937 '
938 'Wait M_In(11269) = 1         '����(F)�V�����_�[�o�[���o
939     fnAutoScreenComment(513)    '��ԕ\��[���i�����҂�] 2022/04/26 �n��
940 MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   '����(F)�V�����_�[�o�[���o
941 If MRtn = 1 Then GoTo *CompFGet1
942 fErrorProcess(11,277,284,0)
943 If M_20# = MNext% Then M_20# = MClear%
944 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
945 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
946 If M_20# = MContinue% Then GoTo *RE_F_GET_1
947 *CompFGet1
948 '
949 Ovrd 25
950 *RE_F_GET_2
951     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
952 Mvs PBracketFGet             '����(F)�󂯎��ʒu
953 '
954 '
955 '
956 M_Out(12249) = 0             '�^��OFF�o���uOFF
957 M_Out(12250) = 0             '�^��j��o���uOFF
958 M_Out(12248) = 1             '�^��ON�o���uON
959 '
960 'Wait M_In(11267) = 1         '����(F)�z���Z���T�[ON���o
961 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '����(F)�z���Z���T�[ON���o
962 If MRtn = 1 Then GoTo *CompFGet2
963 Mvs PBracketFGet_1           '����(F)�󂯎����
964 fErrorProcess(11,280,295,0)  '284��295�ɕύX6/2����
965 If M_20# = MNext% Then M_20# = MClear%
966 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
967 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
968 If M_20# = MContinue% Then GoTo *RE_F_GET_2
969 *CompFGet2
970 '
971 Mvs PBracketFGet_1           '����(F)�󂯎����
972 'Ovrd 100
973 Mov PBracketFGet_2           '����(F)�󂯎����_
974 Mov PBracketFGet_3           '���_
975 '
976 'DVD����(F)��u��
977 Mov PBracketFSet_3           '���_
978 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '����(F)�z���Z���T�[ON������x�m�F
979 Ovrd 100
980 If MRtn = 1 Then GoTo *CompFGet3
981 fErrorProcess(11,280,295,0)  '284��295�ɕύX6/2����
982 If M_20# = MNext% Then M_20# = MClear%
983 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
984 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
985 If M_20# = MContinue% Then GoTo *RE_F_GET_3
986 *CompFGet3
987 '    ' ���i�����v���I��
988     M_Out(12787) = 0
989 '    ' ���i�擾�������M(�p���X)
990     M_Out(12800) = 1 Dly 0.5
991     '
992 'Wait M_In(11892) = 1         '�˂����{3��~5�܂őҋ@
993 MRtn = fScrewTighenRoboCheck(11892)    '��~��Ԃ���M����
994 If MRtn = 0 Then Mov PInitialPosition1  '��~�ʒu�Ɉړ�
995 If MRtn = 0 Then GoTo *ASSY_ERROR_END
996 '
997 '    fnAutoScreenComment(533)    '��ԕ\��[�H���T�̃��{����I���҂�] 2022/04/26 �n��(�R�����g�A�E�g5/13����)
998 'Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�ǉ���������10/1����)(�R�����g�A�E�g5/13����)
999 ''
1000 'M_Out(12912) = 1                  '���u����t���O����(�Փ˖h�~)(�R�����g�A�E�g5/13����)
1001 '
1002     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1003 Mov PBracketFSet_2           '����(F)�u���ʒu���_
1004 Mvs PBracketFSet_1           '����(F)�u���ʒu���
1005 Ovrd 10
1006 Mvs PBracketFSet             '����(F)�u���ʒu
1007 Dly 0.2
1008 '
1009 *RE_F_SET_1
1010 '
1011 M_Out(12248) = 0             '�^��ON�o���uOFF
1012 M_Out(12249) = 1             '�^��OFF�o���uON
1013 M_Out(12250) = 1             '�^��j��o���uON
1014 '
1015 MRtn = frInCheck(11267,0,MSETTIMEOUT05&)
1016 Dly 0.5
1017 'M_Out(12250) = 0             '�^��j��o���uOFF
1018 '
1019 If MRtn = 1 Then GoTo *CompFSet1
1020 M_Out(12250) = 0             '�^��j��o���uOFF
1021 fErrorProcess(11,296,297,0)  '236,284��296,297�ɕύX6/2����
1022 If M_20# = MNext% Then M_20# = MClear%
1023 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1024 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1025 If M_20# = MContinue% Then GoTo *RE_F_SET_1
1026 *CompFSet1
1027 '
1028 '*RE_F_SET_2
1029 'M_Out(12258) = 0             '����(F)�V�����_�[�oOFF
1030 'M_Out(12259) = 1             '����(F)�V�����_�[��ON
1031 ''
1032 ''Wait M_In(11268) = 1         '����(F)�V�����_�[�ߒ[���o
1033 'MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '����(F)�V�����_�[�ߒ[���o
1034 'If MRtn = 1 Then GoTo *CompFSet2
1035 'fErrorProcess(11,278,284,0)
1036 'If M_20# = MNext% Then M_20# = MClear%
1037 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1038 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1039 'If M_20# = MContinue% Then GoTo *RE_F_SET_2
1040 '*CompFSet2
1041 '
1042 Ovrd 1                       '����������h�~(5/13����)
1043 Mvs PBracketFSet , -5        '����������h�~(5/13����)
1044 Ovrd 100                     '����������h�~(5/13����)
1045 Mvs PBracketFSet_1           '����(F)�u���ʒu���
1046 M_Out(12250) = 0             '�^��j��o���uOFF
1047 Ovrd 100
1048 Mvs PBracketFSet_2           '����(F)�u���ʒu���_
1049 Mov PBracketFSet_3           '���_
1050 M_Out(12912) = 0             '���u����t���O���
1051 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~5�`��~6�܂�)
1052 '
1053 *RE_F_SET_2
1054 M_Out(12258) = 0             '����(F)�V�����_�[�oOFF
1055 M_Out(12259) = 1             '����(F)�V�����_�[��ON
1056 '
1057 'Wait M_In(11268) = 1         '����(F)�V�����_�[�ߒ[���o
1058 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '����(F)�V�����_�[�ߒ[���o
1059 If MRtn = 1 Then GoTo *CompFSet2
1060 fErrorProcess(11,278,284,0)
1061 If M_20# = MNext% Then M_20# = MClear%
1062 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1063 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1064 If M_20# = MContinue% Then GoTo *RE_F_SET_2
1065 *CompFSet2
1066 '
1067 '----------PIAS��ǂ�----------
1068 '�T�v�F�ǂ݂ɍs�����ǂ��������߂�Ƃ��Ƀ^�C���A�E�g�Ő�ɐi�߂�悤�ɂ���^�C�}�[
1069     Mov PTicketRead_2
1070     Mov PTicketRead_1
1071     M_22# = MClear%
1072     M_20# = MClear%
1073     M_Timer(4) = 0
1074     MloopFlg = 0
1075     While MloopFlg = 0
1076         MCrtTime& = M_Timer(4)
1077         If M_In(11354) = 1 Then
1078             MloopFlg = 1
1079         ElseIf MCrtTime& > 5000 Then    '�b��5�b(�������Ă���͕̂b���������₷�����Ă����)
1080             MloopFlg = 1
1081         EndIf
1082     WEnd
1083     MRtn = 0
1084     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
1085         If M_In(11354) = 1 Then         '�g���J�n��ON�Ȃ�
1086             M_Out(12346) = 1 Dly 0.5        ' �g���J�n����M
1087             Mvs PTicketRead
1088             MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
1089 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1090 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1091             If MRtn = 1 Then M_22# = MAssyOK%
1092             If M_20# = MContinue% Then M_22# = MContinue%
1093             If M_20# = MPass% Then M_22# = MPass%
1094             If M_20# = MNext% Then M_22# = MPass%
1095         EndIf
1096     EndIf
1097     If M_20# = MNgProcess% Or M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1098     M_20# = MClear%
1099     Mov PTicketRead_1
1100     Mov PTicketRead_2
1101     Mov PBracketFSet_3
1102 '
1103 '�u���ʒu�摜����
1104 'Wait M_In(11893) = 1         '�˂����{3��~6�܂őҋ@
1105 MRtn = fScrewTighenRoboCheck(11893)    '��~��Ԃ���M����
1106 If MRtn = 0 Then Mov PInitialPosition1  '��~�ʒu�Ɉړ�
1107 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1108 '
1109 If M_In(11369) = 0 Then M_Out(12912) = 1    '�摜���薢�g�p�������Ńt���O����6/23����
1110 If M_In(11369) = 0 Then GoTo *SkipCheck1    '�摜���薢�g�p���W�����v
1111 *RE_FLG_SET_1
1112 'Wait M_In(11920) = 0                'BaseUnit5���u����t���O�m�F(�ǉ�2/27����)
1113 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5���u����t���O�m�F(�^�C���A�E�g������ǉ�22/09/29����)
1114 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1115 If MRtn = 2 Then GoTo *RE_FLG_SET_1
1116 M_Out(12912) = 1                    '���u����t���O����
1117 'Wait M_In(11920) = 0              'Ver 0.4 �ǉ�
1118 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5���u����t���O�m�F(�^�C���A�E�g������ǉ�22/09/29����)
1119 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1120 If MRtn = 2 Then GoTo *RE_FLG_SET_1
1121 Dly 0.3
1122 'If M_In(11920) = 1 Then M_Out(12912) = 0
1123 If M_In(11920) = 1 Then GoTo *RE_FLG_SET_1
1124 '
1125 'Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�ǉ���������10/1����)
1126 ''
1127 'M_Out(12912) = 1                  '���u����t���O����(�Փ˖h�~)
1128 '
1129 'Mov PBracketFCheck_2         '�o�H
1130 'Mov PBracketFCheck           '�����ʒu
1131 *RE_F_CHECK
1132 'If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck2
1133 PInspPosition(1) = PBracketFCheck1
1134 MInspGroup%(1) = 4
1135 PInspPosition(2) = PBracketFCheck2
1136 MInspGroup%(2) = 5
1137 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1138 If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
1139 If MRtn = 0 Then
1140     MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1141 EndIf
1142 If MRtn = 1 Then GoTo *CompFCheck
1143 fErrorProcess(11,43,46,0)
1144 If M_20# = MNext% Then M_20# = MClear%
1145 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1146     Mov PBracketFCheck
1147     Mov PInitialPosition
1148     M_Out(12912) = 0
1149 EndIf
1150 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1151 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1152 If M_20# = MContinue% Then GoTo *RE_F_CHECK
1153 *CompFCheck
1154 *SkipCheck1
1155 '
1156 '
1157 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~6�`��~7�܂�)
1158 If M_In(11369) = 1 Then Mov PBracketRCheck1
1159 'Wait M_In(11894) = 1         '�˂����{3��~7�܂őҋ@
1160 MRtn = fScrewTighenRoboCheck(11894)    '��~��Ԃ���M����
1161 If MRtn = 0 Then
1162     Mov PBracketFCheck
1163     Mov PInitialPosition
1164     M_Out(12912) = 0
1165 EndIf
1166 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1167 '
1168 If M_In(11369) = 0 Then GoTo *SkipCheck2    '�摜���薢�g�p���W�����v
1169 *RE_R_CHECK
1170 PInspPosition(1) = PBracketRCheck1
1171 MInspGroup%(1) = 2
1172 PInspPosition(2) = PBracketRCheck2
1173 MInspGroup%(2) = 3
1174 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1175 If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck2
1176 If MRtn = 0 Then
1177     MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1178 EndIf
1179 If MRtn = 1 Then GoTo *CompRCheck
1180 fErrorProcess(11,43,46,0)
1181 If M_20# = MNext% Then M_20# = MClear%
1182 If M_20# = MAbout% Or M_20# =  MNgProcess% Then
1183     Mov PBracketFCheck
1184     Mov PInitialPosition
1185     M_Out(12912) = 0
1186 EndIf
1187 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1188 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1189 If M_20# = MContinue% Then GoTo *RE_R_CHECK
1190 *CompRCheck
1191 *SkipCheck2
1192 '
1193 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~7�`��~8�܂�)�����ʒu�ύX1/20����
1194 '
1195 If M_In(11369) = 1 Then Mov PBracketFCheck
1196 '
1197 '
1198 '�˂����{3��DVDassy�����
1199 'M_Out(12866) = 1 Dly 0.5                   '�˂����{3�ɍĊJ�v��(��~7�`��~8�܂�)�����ʒu�ύX1/20����
1200 Mov PMechaOnRoboGet_3                       '�������킹
1201 'M_Out(12912) = 0                            '���u����t���O���(6/23�b��R�����g�A�E�g(����))
1202 '
1203 Mov PMechaOnRoboGet_2                       '���_(�����ʒu�ύX1/20����)
1204 '
1205 'Wait M_In(11895) = 1                       '�˂����{3��~8�܂őҋ@
1206 MRtn = fScrewTighenRoboCheck(11895)         '��~��Ԃ���M����
1207 If MRtn = 0 Then Mov PInitialPosition1      '��~�ʒu�Ɉړ�
1208 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1209 *RE_FLG_SET_2
1210     fnAutoScreenComment(533)    '��ԕ\��[�H���T�̃��{����I���҂�] 2022/04/26 �n��
1211 'Wait M_In(11920) = 0                        'BaseUnit5���u����t���O�m�F(�����ǉ�2/9����)
1212 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5���u����t���O�m�F(�^�C���A�E�g������ǉ�22/09/29����)
1213 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1214 If MRtn = 2 Then GoTo *RE_FLG_SET_2
1215 '
1216 '
1217 M_Out(12912) = 1                            '���u����t���O����(�Փ˖h�~)
1218 Dly 0.3
1219 'Wait M_In(11920) = 0                        'Ver 0.4 �ǉ��@�H��6�D��
1220 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5���u����t���O�m�F(�^�C���A�E�g������ǉ�22/09/29����)
1221 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1222 If MRtn = 2 Then GoTo *RE_FLG_SET_2
1223 '
1224 'If M_In(11920) = 1 Then M_Out(12912) = 0   'Ver 0.4 �R�����g�A�E�g
1225 If M_In(11920) = 1 Then GoTo *RE_FLG_SET_2
1226 '
1227 'Mov PMechaOnRoboGet_2                      '�����ʒu�ύX(1/20����)
1228 Mov PMechaOnRoboGet_1                       '�˂����{���
1229 Ovrd 25
1230 '
1231 *RE_ROBO_GET_1
1232 '
1233     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1234 Mvs PMechaOnRoboGet          'DVD���J�󂯎��ʒu
1235 M_Out(12257) = 0             'DVD�`���b�N�JOFF
1236 M_Out(12256) = 1             'DVD�`���b�N��ON
1237 '
1238 'Wait M_In(11266) = 1         'DVD���J�`���b�N���o
1239 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1240 If MRtn = 1 Then GoTo *CompRoboGet1
1241 fErrorProcess(11,269,284,0)
1242 If M_20# = MNext% Then M_20# = MClear%
1243 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1244 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1245 If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1246 *CompRoboGet1
1247 '
1248 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~8�`��~9�܂�)
1249 '
1250 'Wait M_In(11876) = 1         '�˂����{3�˂����ߊ�������M
1251 '
1252 'Wait M_In(11896) = 1         '�˂����{3��~9�܂őҋ@
1253 MRtn = fScrewTighenRoboCheck(11876)    '��~��Ԃ���M����
1254 'If MRtn = 0 Then
1255 '    M_Out(12256) = 0             'DVD���J�`���b�N��OFF
1256 '    M_Out(12257) = 1             'DVD���J�`���b�N�JON
1257 '    Mvs PMechaOnRoboGet_1
1258 '    Mov PMechaOnRoboGet_2
1259 '    Mov PInitialPosition
1260 'EndIf
1261 If MRtn = 0 Then GoTo *ASSY_ERROR_END   '���̏�Œ�~
1262 '
1263 'Wait M_In(11264) = 1         'DVD���J���o
1264 *RE_ROBO_GET_2
1265 MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   'DVD���J���o
1266 If MRtn = 1 Then GoTo *CompRoboGet2
1267 fErrorProcess(11,273,284,0)
1268 If M_20# = MNext% Then M_20# = MClear%
1269 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1270 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1271 If M_20# = MContinue% Then GoTo *RE_ROBO_GET_2
1272 *CompRoboGet2
1273 '
1274 Mvs PMechaOnRoboGet_1        '�˂����{���
1275 Ovrd 100
1276 Mov PMechaOnRoboGet_2        '�˂����{���_
1277 'M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~9�`�˂����ߊ����܂�)
1278 M_Out(12912) = 0                  '���u����t���O���(�ǉ�10/1����)
1279 '
1280 Mov PTicketRead_2
1281 M_Out(12868) = 1 Dly 0.5     '�˂����{3�˂����ߊ����𑗐M(�����ʒu�ύX3/26����)
1282 '
1283 '
1284     If M_22# = MAssyOK% Then GoTo *CompRead
1285 *IRREGULAR      '�J�n��Assy������DVD���J�c���W�����v��
1286     Mov PTicketRead_1
1287 '
1288 'DVDassy���p���b�g�֒u��
1289 '    Wait M_In(11218) = 1         '�p���b�g���㏸���Ă��邱�Ƃ��m�F
1290     If M_22# <> MClear% And M_22# <> MIrregular% Then GoTo *RE_PIAS_CHECK
1291     fnAutoScreenComment(95)    '��ԕ\��[�p���b�g�����ҋ@��] 2022/04/26 �n��
1292     Wait M_In(11354) = 1         ' �g���J�n�M�����o�Ă��邱�Ƃ��m�F
1293     M_Out(12346) = 1 Dly 0.5         ' �g���J�n����M
1294     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1295 *RE_PIAS_CHECK
1296     M_20# = MClear%                 '������
1297     MRtn = 1
1298     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
1299         If M_22# = MClear% Or M_22# = MContinue% Or M_22# = MIrregular% Then'PIAS�`�F�b�N�ɂĖ����������g���C���ɓ���
1300                 Mvs PTicketRead
1301                 MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
1302 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1303 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1304         EndIf
1305     EndIf
1306 '
1307     If M_22# = MPass% Then M_20# = MPass%
1308     M_22# = MClear%
1309     If MRtn = 1 And M_20# = MClear% Then GoTo *CompRead
1310 '    fErrorProcess(11,17,0,0)
1311 '    Dly 10                                      '�f�o�b�O�p
1312 '    If M_In(11359) = 1 Then M_22# = MIrregular%'�f�o�b�O�p
1313 '    If M_22# = MIrregular% Then *ASSY_ERROR_END  '�f�o�b�O�p
1314 '    If M_20# = MPass% Then M_20# = MClear%      '�f�o�b�O�p
1315     If M_20# = MPass% Then
1316         M_22# = MIrregular%
1317         M_20# = MAssyOK%
1318         Dly 0.1
1319         Mvs PTicketRead_1                         '22/04/26 �ǉ� �n��
1320     EndIf
1321     If M_20# = MAssyOK% Then GoTo *AssyEnd
1322     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1323     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1324     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1325     If M_20# = MContinue% Then GoTo *RE_PIAS_CHECK
1326 *CompRead
1327 '
1328 'Mov PTicketRead_1
1329 Accel 25 , 25                '�f�o�b�O��
1330 Mov PMechaOnPltSet_2         '�p���b�g���_
1331 Accel 100 , 100
1332 Mov PMechaOnPltSet_1         '�p���b�g���
1333 '
1334 '�u���O��DVD�������Ă��邩�H�m�F    2022/04/12 �n��
1335 'DVD�������Ă����炸�A�`���b�N�̏ꍇ�A�[�̐M����ON���Ȃ�
1336 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1337 If MRtn = 1 Then GoTo *DVDCheckEnd         '�`���b�N�̕[��ON�̏ꍇ
1338 fErrorProcess(11,269,284,0)
1339 If M_20# = MNext% Then M_20# = MClear%
1340 If M_20# = MClear% Then GoTo *CompPltSet
1341 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1342 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1343 If M_20# = MContinue% Then GoTo *CompRead
1344 *DVDCheckEnd
1345 '
1346 Dly 0.2
1347 Ovrd 10
1348 Mvs PMechaOnPltSet           'DVD���J�u���ꏊ
1349 Dly 0.2
1350 '
1351 *RE_PLT_SET
1352 '
1353 M_Out(12256) = 0             'DVD���J�`���b�N��OFF
1354 M_Out(12257) = 1             'DVD���J�`���b�N�JON
1355 '
1356 'Wait M_In(11265) = 1         'DVD���J�`���b�N�J���o
1357 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1358 If MRtn = 1 Then GoTo *CompPltSet
1359 fErrorProcess(11,270,284,0)
1360 If M_20# = MNext% Then M_20# = MClear%
1361 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1362 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1363 If M_20# = MContinue% Then GoTo *RE_PLT_SET
1364 *CompPltSet
1365 M_22# = MClear%
1366 '
1367 Mvs PMechaOnPltSet_1         '�p���b�g���
1368 Ovrd 100
1369     MRtn = FnCtlValue2(2)          '�g���n�j�{�P  2022/04/28 �n��
1370 Mov PMechaOnJigGet_3
1371     MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1372 'Mov PInitialPosition   '�R�����g�A�E�g(1/20����)
1373 '
1374 'Wait M_In(11876) = 1         '�˂����{3�˂����ߊ�������M
1375 'MRtn = frInCheck(11876,1,MSETTIMEOUT05&)   '�˂����{3�˂����ߊ�������M�E
1376 'If MRtn = 0 Then
1377 '    fErrorProcess()         '�G���[����
1378 'EndIf
1379 'M_Out(12868) = 1 Dly 0.5     '�˂����{3�˂����ߊ����𑗐M(�����ʒu�ύX3/26����)
1380 '�`�P�b�gID��������
1381 'If M_20# <> MPass% Then M_20# = MAssyOK%
1382 M_20# = MAssyOK%
1383 '
1384 *ASSY_ERROR_END
1385 *AssyEnd
1386 *fnAssyStart_FEndPosi
1387 FEnd
1388 '
1389 '��fnPiasCheck
1390 ''' <summary>
1391 ''' PIAS�`�P�b�g�Ǎ���
1392 ''' </summary>
1393 ''' <returns>   0 : NG
1394 '''             1 : OK(�Ǎ��݊���)
1395 ''' </returns>
1396 ''' <remarks>
1397 ''' Date   : 2021/07/07 : M.Hayakawa
1398 ''' </remarks>'
1399 Function M% fnPiasCheck
1400     fnPiasCheck = 0
1401     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1402     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1403 '
1404 *RETRY_PIAS
1405     M_20# = MClear%
1406     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1407     '
1408     '�yID�`�P�b�g�ǂݍ��݁z
1409     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1410     MInspGroup%(1) = 1              '����G�ԍ�
1411     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1412 '
1413     '�G���[�̏ꍇ
1414     If MRtn <> 1 Then
1415         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1416         If MRtn <> 1 Then
1417             'D720 -> D1300 �R�s�[�v��
1418             M_Out(12565) = 1
1419             Dly 0.5
1420             M_Out(12565) = 0
1421             '�G���[�����L�q
1422             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1423             'GOT KEY���͑҂�
1424             MKeyNumber = fnKEY_WAIT()
1425         '
1426             Select MKeyNumber
1427                 Case MNext%         '���ւ�I�������ꍇ
1428                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1429                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1430 '                    GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��(�֐��O���ŕ��򂷂�悤�ύX3/26����)
1431                     Break
1432                 Case MAbout%        '��~��I�������ꍇ
1433                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1434                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1435 '                    GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��(�֐��O���ŕ��򂷂�悤�ύX3/26����)
1436                     Break
1437                 Case MNgProcess%    'NG��I�������ꍇ
1438                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1439                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1440 '                    GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��(�֐��O���ŕ��򂷂�悤�ύX3/26����)
1441                     Break
1442                 Case MContinue%     '�p����I�������ꍇ
1443                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1444                     M_20# = MContinue%
1445 '                    GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C(�֐��O���ŕ��򂷂�悤�ύX3/26����)
1446                     Break
1447             End Select
1448         EndIf
1449     EndIf
1450     If M_20# <> MClear% Then GoTo *fnPiasCheck_End
1451 '
1452 '----------D720 -> D1300 �R�s�[�v��----------
1453     M_Out(12565) = 1
1454     Dly 0.5
1455     M_Out(12565) = 0
1456 '----------�ʐM�m�F������----------
1457     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1458     MRtn = 0                ' ������
1459     M_20# = MClear%         ' ������
1460     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1461     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j(�֐��O���ŕ��򂷂�悤�ɕύX3/26����)
1462     If MRtn <> 1 Then GoTo *fnPiasCheck_End
1463 '        If M_20# = MContinue% Then
1464 '            GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1465 '        Else
1466 '            GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1467 '        EndIf
1468 '    EndIf
1469 '----------�H�������m�F----------
1470     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1471     MRtn = 0                ' ������
1472     M_20# = MClear%         ' ������
1473     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1474     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j(�֐��O���ŕ��򂷂�悤�ɕύX3/26����)
1475     If MRtn <> 1 Then GoTo *fnPiasCheck_End
1476 '        If M_20# = MContinue% Then
1477 '            GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1478 '        Else
1479 '            GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1480 '        EndIf
1481 '    EndIf
1482     '
1483     fnPiasCheck = 1
1484     *fnPiasCheck_End
1485 FEnd
1486 '
1487 '��fnPCComuCheck
1488 ''' <summary>
1489 ''' PC-PLC�ʐM�`�F�b�N
1490 ''' </summary>
1491 ''' <returns>   0 : NG
1492 '''             1 : OK(�Ǎ��݊���)
1493 ''' </returns>
1494 ''' <remarks>
1495 ''' Date   : 2021/07/07 : M.Hayakawa
1496 ''' </remarks>'
1497 Function M% fnPCComuCheck
1498     fnPCComuCheck = 0
1499     MJudge% = 0                                  '������
1500     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1501     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1502     '
1503     For MStaNo = 0 To 5
1504         '
1505         If M_In(MIN_PIAS_ComOK%) = 1 Then
1506             'PC�ʐMOK(M400)
1507             MJudge% = MOK%
1508             MStaNo = 5
1509             Break
1510         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1511             'toRBT_�ʐM�m�Ftime out
1512             MJudge% = MNG%
1513             MCommentD1001 = 15
1514             MCommentD1002 = 21
1515             MStaNo = 5
1516             Break
1517         Else
1518             'toRBT_�ʐM�m�Ftime out
1519             MJudge% = MNG%
1520             MCommentD1001 = 14
1521             MCommentD1002 = 21
1522             Break
1523         EndIf
1524     Next MStaNo
1525     '
1526     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1527     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1528     '
1529     '�G���[���
1530     If MJudge% <> MOK% Then
1531         M_20# = MClear%     '������
1532         '�G���[�����L�q
1533         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1534         'GOT KEY���͑҂�
1535         MKeyNumber = fnKEY_WAIT()
1536         '
1537         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1538             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1539             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1540             Break
1541         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1542             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1543             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1544             Break
1545         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1546             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1547             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1548             Break
1549         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1550             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1551             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1552             Break
1553         EndIf
1554     Else
1555         'OK�̏ꍇ
1556         fnPCComuCheck = 1
1557     EndIf
1558 FEnd
1559 '
1560 '��fnProcessCheck
1561 ''' <summary>
1562 ''' �H�������m�F
1563 ''' </summary>
1564 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1565 '''             -1�F�O�H������NG  -2�F���H����������
1566 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1567 '''             -5�F���������G���[
1568 ''' </returns>
1569 ''' <remarks>
1570 ''' Date   : 2021/07/07 : M.Hayakawa
1571 ''' </remarks>'
1572 Function M% fnProcessCheck
1573     fnProcessCheck = 0
1574     MJudge% = MNG%      '��UNG���������Ƃ���
1575 '----------�H�������m�F----------
1576     MCommentD1001 = 0   '�R�����g������
1577     For MStaNo = 0 To 5
1578         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1579         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1580         '
1581         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1582             MJudge% = MOK%
1583             fnAutoScreenComment(85)     ' AUTO���
1584             MStaNo = 5
1585             Break
1586         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1587             MFlgLoop% = 0
1588             MJudge% = MNG%
1589             MCommentD1001 = 27
1590             MCommentD1002 = 22
1591             fnAutoScreenComment(94)     ' AUTO���
1592             fnProcessCheck = -2         ' NG��-2��Ԃ�
1593             MStaNo = 5
1594             Break
1595         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1596            MJudge% = MNG%
1597             MCommentD1001 = 31
1598             MCommentD1002 = 22
1599             fnAutoScreenComment(83)     ' AUTO���
1600             fnProcessCheck = -3         ' NG��-3��Ԃ�
1601             MStaNo = 5
1602             Break
1603         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1604             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1605             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1606             MJudge% = MNG%
1607             MCommentD1001 = 32
1608             MCommentD1002 = 22
1609             fnAutoScreenComment(84)     ' AUTO���
1610             fnProcessCheck = -1         ' NG��-1��Ԃ�
1611             Dly 1.0
1612             '�H�������m�FOFF
1613             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1614             Dly 1.0
1615            'MStaNo = 5
1616             Break
1617         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1618             MFlgLoop% = 0
1619             MJudge% = MNG%
1620             MCommentD1001 = 29
1621             MCommentD1002 = 22
1622             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1623             fnProcessCheck = -5         ' NG��-5��Ԃ�
1624             MStaNo = 5
1625             Break
1626         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1627             MJudge% = MNG%
1628             If MCommentD1001 = 32 Then
1629                 '�������Ȃ�
1630             Else
1631                 MCommentD1001 = 26
1632             EndIf
1633             MCommentD1002 = 22
1634             fnProcessCheck = -4         ' NG��-4��Ԃ�
1635             MStaNo = 5
1636             Break
1637         Else
1638             MJudge% = MNG%
1639             MCommentD1001 = 28
1640             MCommentD1002 = 22
1641         EndIf
1642     Next MStaNo
1643     '�H�������m�FOFF
1644     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1645     '�ʉߗ���NG �H�������̏ꍇ
1646     If MJudge% = MPass% Then
1647         M_20# = MPass%
1648     EndIf
1649     '
1650     '�G���[���
1651     If MJudge% <> MOK% Then
1652         M_20# = MClear%     '������
1653         '�G���[�����L�q
1654         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1655         'GOT KEY���͑҂�
1656         MKeyNumber = fnKEY_WAIT()
1657         '
1658         Select MKeyNumber
1659             Case MAbout%        '��~��I�������ꍇ
1660                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1661                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1662                 Break
1663             Case MNext%         '���ւ�I�������ꍇ
1664                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1665                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1666                 Break
1667             Case MContinue%     '�p����I�������ꍇ
1668                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1669                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1670                 Break
1671             Case MNgProcess%    'NG��I�������ꍇ
1672                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1673                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1674                 Break
1675         End Select
1676     Else
1677         fnProcessCheck = 1  ' OK��1��Ԃ�
1678     EndIf
1679 FEnd
1680 '
1681 '��fnPiasWrite
1682 ''' <summary>
1683 ''' Pias �g�����ʏ����ݗv��
1684 ''' </summary>
1685 '''<param name="MFlg%">
1686 '''                 MOK%(1) = �H��������OK��������
1687 '''                 MNG%(0) = �H��������NG��������
1688 '''</param>
1689 '''<returns></returns>
1690 ''' <remarks>
1691 ''' Date   : 2021/07/07 : M.Hayakawa
1692 ''' </remarks>'
1693 Function M% fnPiasWrite(ByVal MFlg%)
1694       fnPiasWrite = 0
1695 *RETRY_PIASWRITE
1696     '
1697     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1698    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1699     If MFlg% = MOK% Then
1700         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1701     Else
1702         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1703     EndIf
1704     Dly 0.1                  '�O�̂���
1705     '
1706     'Pias�֏����݊J�n M305 -> ON
1707     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1708     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1709     '
1710     MJudge% = MNG%
1711     '
1712     For MStaNo = 0 To 5
1713         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1714             MJudge% = MOK%
1715             'MRet = fnAutoScreenComment(85)  'AUTO���
1716             MStaNo = 5
1717             Break
1718         '
1719         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1720             MJudge% = MNG%
1721             'MRet = fnAutoScreenComment(85)  'AUTO���
1722            MCommentD1001 = 34
1723            MCommentD1002 = 25
1724             MStaNo = 5
1725             Break
1726         '
1727         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1728             MJudge% = MNG%
1729             'MRet = fnAutoScreenComment(85)  'AUTO���
1730            MCommentD1001 = 35
1731            MCommentD1002 = 25
1732             MStaNo = 5
1733             Break
1734         '
1735         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1736             MJudge% = MNG%
1737             'MRet = fnAutoScreenComment(85)  'AUTO���
1738            MCommentD1001 = 36
1739            MCommentD1002 = 25
1740             MStaNo = 5
1741             Break
1742         '
1743         Else
1744             MJudge% = MNG%
1745            MCommentD1001 = 42
1746            MCommentD1002 = 25
1747         '
1748         EndIf
1749         '
1750     Next MStaNo
1751     '
1752     'Pias�֏����݊J�n M305 -> OfF
1753     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1754     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1755     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1756     '
1757     '
1758     '�ʉߗ���NG �H�������̏ꍇ
1759     If MJudge% = MPass% Then
1760         M_20# = MPass%
1761     EndIf
1762     '
1763    M_20# = MClear%     '������
1764     '
1765     '�G���[���
1766     If MJudge% < MOK% Then
1767     '
1768 '�c���Ă���������ł͎g�p���Ȃ����x��
1769 *RETRY_ERR_WRITE
1770         M_20# = MClear%     '������
1771         '�G���[�����L�q
1772         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1773         'GOT KEY���͑҂�
1774         MKeyNumber = fnKEY_WAIT()
1775         '
1776         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1777             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1778            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1779             Break
1780         '
1781         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1782             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1783             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1784         '
1785         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1786             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1787             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1788         '
1789         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1790             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1791            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1792             Break
1793         '
1794         EndIf
1795         '
1796         If M_20# = MClear% Then *RETRY_ERR_WRITE
1797         '
1798     EndIf
1799     '
1800     If M_20# = MContinue% Then *RETRY_PIASWRITE
1801     '
1802     fnPiasWrite = 1
1803     '
1804 FEnd
1805 '
1806 '��fnPCBNumberCheck
1807 ''' <summary>
1808 ''' Pias ��ԍ��ƍ��v��
1809 ''' </summary>
1810 '''<param name="%"></param>
1811 '''<param name="%"></param>
1812 '''<returns></returns>
1813 ''' <remarks>
1814 ''' Date   : 2021/07/07 : M.Hayakawa
1815 ''' </remarks>'
1816 Function M% fnPCBNumberCheck
1817       fnPCBNumberCheck = 0
1818     '
1819 *RETRY_PCBCHECK
1820     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1821     'Pias�֊�ƍ��J�n M310 -> ON
1822     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1823     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1824     '
1825     MJudge% = MNG%
1826     '
1827     For MStaNo = 0 To 5
1828         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1829             MJudge% = MOK%
1830             fnAutoScreenComment(96)  'AUTO���
1831             MStaNo = 5
1832             Break
1833         '
1834         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1835             MJudge% = MNG%
1836             fnAutoScreenComment(97)  'AUTO���
1837             MCommentD1001 = 37
1838             MCommentD1002 = 25
1839             MStaNo = 5
1840             Break
1841         '
1842         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1843             MJudge% = MNG%
1844             fnAutoScreenComment(98)  'AUTO���
1845             MCommentD1001 = 38
1846             MCommentD1002 = 25
1847             MStaNo = 5
1848             Break
1849         '
1850         ElseIf M_In(11580) = 1 Then                         'time out
1851             MJudge% = MNG%
1852             fnAutoScreenComment(99)  'AUTO���
1853             MCommentD1001 = 39
1854             MCommentD1002 = 25
1855             MStaNo = 5
1856             Break
1857         '
1858         Else
1859             MJudge% = MNG%
1860            MCommentD1001 = 41
1861            MCommentD1002 = 25
1862         '
1863         EndIf
1864         '
1865     Next MStaNo
1866     '
1867     'Pias�֊�ƍ��J�n M310 -> OfF
1868     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1869     '
1870     '
1871     '�ʉߗ���NG �H�������̏ꍇ
1872     If MJudge% = MPass% Then
1873         M_20# = MPass%
1874     EndIf
1875     '
1876    M_20# = MClear%     '������
1877     '
1878     '�G���[���
1879     If MJudge% < MOK% Then
1880     '
1881 '�c���Ă���������ł͎g�p���Ȃ����x��
1882 *RETRY_ERR_PCBNUMBER
1883         M_20# = MClear%     '������
1884         '�G���[�����L�q
1885         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1886         'GOT KEY���͑҂�
1887         MKeyNumber = fnKEY_WAIT()
1888         '
1889         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1890             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1891             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1892             Break
1893         '
1894         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1895             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1896             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1897         '
1898         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1899             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1900             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1901         '
1902         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1903             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1904             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1905             Break
1906         '
1907         EndIf
1908         '
1909         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1910         '
1911     EndIf
1912     '
1913     If M_20# = MContinue% Then *RETRY_PCBCHECK
1914 FEnd
1915 '
1916 '��ScrewTight_S2
1917 ''' <summary>
1918 ''' �˂����߂��s��
1919 ''' </summary>
1920 '''<param name="PScrewPos()">
1921 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1922 '''             PScrewPos(2)    �F�˂����߉��_
1923 '''             PScrewPos(10)   �F�˂����ߏI������
1924 '''</param>
1925 '''<returns>����
1926 '''         0=�ُ�I���A1=����I��
1927 '''</returns>
1928 ''' <remarks>
1929 ''' Date   : 2021/07/07 : M.Hayakawa
1930 ''' </remarks>'
1931 Function M% ScrewTight_S2(ByVal PScrewPosition())   '�l�W���ߌʐݒ�
1932     ScrewTight_S2 = 0
1933     MOKNGFlg = 0
1934     Ovrd 100
1935     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1936     ' �b��
1937     Ovrd 5
1938     Mvs PScrewPosition(10),-10    ' �p���b�g��˂�����S�@�̏��ֈړ�
1939 '    Ovrd MOvrdA
1940     '�b��}�X�N
1941 '    M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
1942 '    Dly 0.1
1943 '    M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1944 '    'Spd 8.3 '�O�����C�h100  �������C�h60   '���C�h100-40�@100%�FSpd�@15�@'�˂����ߑ��x�ݒ�
1945 '    Spd MSpdA               '�l�W���ߎ�Spd�ʐݒ�
1946     ' �b��ړ��̂�
1947     Mvs PScrewPosition(10)
1948 '    '
1949 '    Dly 0.1
1950 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1951 '    Wait M_In(11584)=1          '����/�G���[���o
1952 '    Dly 0.1
1953 '    Spd M_NSpd
1954 '    '
1955 '    If M_In(X28_Driver)=1 Then  '�˂��g�[�^���G���[���o��
1956 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1957 '        Dly 0.1
1958 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1959 '        Dly 0.1
1960 '        M_Out(Y63_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1961 '        Dly 0.1
1962 '        M_Out(Y65_Driver)=0     '�v���O���������@F1
1963 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1964 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
1965 '        MOKNGFlg = -1
1966 '        ScrewTight_S2 = 0
1967 '    Else
1968 '        Wait M_In(X29_Driver)=1 ' ���튮����
1969 '        Dly 0.1
1970 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1971 '        Dly 0.1
1972 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O����
1973 '        Dly 0.1
1974 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
1975 '        Dly 0.1
1976 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1977 '        ScrewTight_S2 = 1
1978 '    EndIf
1979 ' �b��
1980     Ovrd 10
1981     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1982     Ovrd 100
1983 FEnd
1984 '
1985 '��ScrewGet_S3
1986 ''' <summary>
1987 ''' �˂������@����˂��𓾂�
1988 ''' </summary>
1989 '''<param name="%"></param>
1990 '''         PScrewPos(1)    �F�˂�������̂˂����
1991 '''         PScrewPos(2)    �F�˂���������_
1992 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1993 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1994 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1995 '''<returns>����
1996 '''         0=�ُ�I���A1=����I���A-1=M�l�W�Z���T�[NG�A-2=M�l�W�Z���T�[ON�A-3=�z���G���[
1997 '''</returns>
1998 ''' <remarks>
1999 ''' Date   : 2021/07/07 : M.Hayakawa
2000 ''' </remarks>'
2001 Function M% ScrewGet_S3(ByVal PScrewPosition())
2002     ScrewGet_S3 = 0
2003     MMScrewJudge% = 0
2004     '�˂������평������G���[�`�F�b�N
2005 ' ���b��폜
2006 '    Wait M_In(X34_ScrewReady1)=1 '�˂�������S��Ready�ɂȂ�܂ő҂@���@���b���҂���Ready�ɂȂ�Ȃ���Δ�����v���O�������K�v�H
2007 '    Ovrd 100
2008 '    If M_In(X33_SS2)=0 Then  'M�˂����o�Z���T��OFF�i�̏�j���Ă����ꍇ
2009 '        Ovrd 30
2010 '        Mvs,-80             '���̏ꏊ����80mm���ֈړ�
2011 '        Mov PInitPos19049   '19049�����ʒu�ֈړ�
2012 '        M_Out(Y6A_VV1)=0    '�˂��z�� Off
2013 '        'NG�Ƃ��Ă����̊֐����甲����
2014 '        ScrewGet_S3 = -1
2015 '        MMScrewJudge% = 1
2016 '        MCommentD1001 = 61
2017 '    EndIf
2018 '    If ScrewGet_S3 = 0 Then
2019 '        'S�^�C�g�p�˂������@��M�˂����������Ă��Ȃ����Ď�
2020 '        MMScrewJudge% = 0 'MMScrewJudge������������
2021 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT05&)
2022 '        If MRtn = 0 Then
2023 '            Ovrd 30
2024 '            Mvs,-80            '���̏ꏊ����50mm���ֈړ�
2025 '            Mov PInitPos19049  '19049�����ʒu�ֈړ�
2026 '            MMScrewJudge% = 2
2027 '            MRtn = All_CLamp_Release()'�S�ẴN�����v�����֕���
2028 '            MCnt% = 2   '2��ݒ�
2029 '            MCommentD1001 = 62
2030 '        EndIf
2031 '        If MMScrewJudge% = 2 Then
2032 '            ScrewGet_S3 = -2
2033 '        EndIf
2034 '    EndIf
2035 '    'M�l�W���肪ON�̏ꍇ NG�Ƃ��Ċ֐��𔲂���
2036 '    If MMScrewJudge% = 2 Then
2037 '        ScrewGet_S3 = -2
2038 '    EndIf
2039     'S�l�W�p�˂����Y��M�l�W�����m�F�p�����܂�
2040     Ovrd 100
2041     Spd M_NSpd
2042     If MMScrewJudge% = 0 Then
2043         ScrewGet_S3 = 0
2044         M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
2045         MScrewCnt% = 0
2046         MFinCnt% = 2
2047 '        For MCnt% = 0 To MFinCnt%
2048             Mov PScrewPosition(2)        ' �˂������@���_
2049             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
2050             Ovrd 80
2051             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2052             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
2053             Mvs PScrewPosition(10), 1.2
2054             M_Out(Y6A_VV1)=1        ' �˂��z���@ON
2055             '�r�b�g��]
2056             M_Out(Y60_Driver)=1
2057             Dly 0.2
2058             '
2059             Ovrd 100
2060             JOvrd M_NJovrd
2061             Spd M_NSpd
2062             '�l�W�z���m�F�ʒu�ړ�
2063             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
2064             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
2065             '�r�b�g��]��~
2066             'M_Out(Y60_Driver)=0
2067             '
2068             '1�b�ԃl�W�z���m�F
2069 ' �ȉ��b��폜
2070 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2071 '            'MRtn = 0'�����G���[
2072 '            '�z���G���[�̏ꍇ
2073 '            '�l�W���˂����Y�ɖ߂�
2074 '            If MRtn = 0 Then
2075 '                Ovrd 30
2076 '                '�r�b�g��]��~
2077 '                M_Out(Y60_Driver)=0
2078 '                '�l�W�����@���
2079 '                Mvs PScrewPos(1)
2080 '                '�X�ɏ��
2081 '                Mov PScrewPos(1), -75
2082 '                '�l�W�̂Ĉʒu
2083 '                Mov PScrewFeedS021
2084 '                '�z��OFF
2085 '                M_Out(Y6A_VV1)=0 '�˂��z���@OFF
2086 '                Dly 0.2
2087 '                '�j��ON
2088 '                M_Out(Y6B_VB1)=1 '�^��j��ON
2089 '                '�r�b�g��]
2090 '                M_Out(Y61_Driver)=1
2091 '                Dly 0.5
2092 '                '
2093 '                Ovrd 100
2094 '                JOvrd M_NJovrd
2095 '                Spd M_NSpd
2096 '                '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
2097 '                Mov PScrewFeedS021, 10
2098 '                Mov PScrewFeedS021
2099 '                Dly 0.1
2100 '                Mov PScrewFeedS021, 10
2101 '                Mov PScrewFeedS021
2102 '                '
2103 '                '�l�W�����҂�
2104 '                '�r�b�g��]��~
2105 '                M_Out(Y61_Driver)=0
2106 '                Dly 0.1
2107 '                '�j��OFF
2108 '                M_Out(Y6B_VB1)=0 '�^��j��OFF
2109 '                '
2110 '                '
2111 '                '�˂��������Ƃ��āA�ړ��X�ɏ��
2112 '                Mov PScrewPos(1), -75
2113 '                Ovrd 100
2114 '                Spd M_NSpd
2115 '                '�l�W�����@���
2116 '                Mvs PScrewPos(1)
2117 '                '
2118 '                ScrewGet_S3 = -3
2119 '                Break
2120 '                '
2121 '            Else
2122 '                MCnt% = MFinCnt%
2123 '                ScrewGet_S3 = 0
2124 '            EndIf
2125 '        Next  MCnt%
2126         '
2127         Ovrd 100
2128         Spd M_NSpd
2129         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2130         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
2131         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
2132         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2133         '������x�z���m�F
2134 ' �ȉ��b��폜
2135 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2136 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
2137 '            MCommentD1001 = 94
2138 '            MCommentD1002 = 95
2139 '            ScrewGet_S3 = -3
2140 '        EndIf
2141 '        If MRtn = 1 Then      '�z��OK�̏ꍇ
2142 '            ScrewGet_S3 = 1
2143 '        EndIf
2144 '        Break
2145     Else
2146         'M�l�W
2147         If MMScrewJudge% = 2 Then
2148             ScrewGet_S3 = -2
2149         EndIf
2150     EndIf
2151 FEnd
2152 '
2153 '��fnKEY_WAIT()
2154 ''' <summary>
2155 ''' GOT����̃L�[���͑҂�
2156 ''' </summary>
2157 '''<returns>1�F��~    2�F����
2158 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
2159 '''         5�FNG
2160 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
2161 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
2162 '''</returns>
2163 ''' <remarks>
2164 ''' Date   : 2021/07/07 : M.Hayakawa
2165 ''' </remarks>'
2166 Function M% fnKEY_WAIT()
2167     fnKEY_WAIT = 0
2168     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
2169     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
2170     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
2171     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2172     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
2173     Dly 0.2
2174     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
2175     MLocalLoopFlg=1
2176     While MLocalLoopFlg=1
2177         If M_In(11345) = 1 Then         '��~   M5345
2178             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
2179             fnKEY_WAIT = 1
2180             MLocalLoopFlg=-1
2181             Break
2182         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
2183             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
2184             fnKEY_WAIT = 2
2185             MLocalLoopFlg=-1
2186             Break
2187         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
2188             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
2189             fnKEY_WAIT = 3
2190             MLocalLoopFlg=-1
2191             Break
2192         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
2193             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
2194             fnKEY_WAIT = 4
2195             MLocalLoopFlg=-1
2196             Break
2197         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
2198             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
2199             fnKEY_WAIT = 5
2200             MLocalLoopFlg=-1
2201             Break
2202             '
2203         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
2204             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
2205             fnKEY_WAIT = MRobotInit1%
2206             MLocalLoopFlg=-1
2207             Break
2208             '
2209         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
2210             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
2211             fnKEY_WAIT = MRobotInit2%
2212             MLocalLoopFlg=-1
2213             Break
2214             '
2215         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
2216             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
2217             fnKEY_WAIT = MRobotInit3%
2218             MLocalLoopFlg=-1
2219             Break
2220             '
2221         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2222             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2223             fnKEY_WAIT = MRobotInit4%
2224             MLocalLoopFlg=-1
2225             Break
2226             '
2227         Else
2228         EndIf
2229     WEnd
2230     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2231     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2232 FEnd
2233 '
2234 '�� fnAUTO_CTL
2235 ''' <summary>
2236 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2237 ''' </summary>
2238 ''' <remarks>
2239 ''' Date   : 2021/07/07 : M.Hayakawa
2240 ''' </remarks>
2241 Function M% fnAUTO_CTL
2242     fnAUTO_CTL = 0
2243     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2244     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2245     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2246     '
2247     If M_Svo=0 Then             '�T�[�{ON�m�F
2248         Servo On
2249     EndIf
2250     Wait M_Svo=1
2251 FEnd
2252 '
2253 '�� fnWindScreenOpen
2254 ''' <summary>
2255 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2256 ''' </summary>
2257 '''<param name="%"></param>
2258 '''<param name="%"></param>
2259 '''<param name="%"></param>
2260 '''<param name="%"></param>
2261 ''' <remarks>
2262 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2263 ''' MWindReSet = 0     ��ʔ�\��
2264 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2265 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2266 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2267 ''' Date   : 2021/07/07 : M.Hayakawa
2268 ''' </remarks>
2269 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2270     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2271         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2272     EndIf
2273     '
2274     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2275         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2276     EndIf
2277     '
2278     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2279        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2280     EndIf
2281     '
2282     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2283     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2284     Dly 0.5
2285     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2286 FEnd
2287 '
2288 '��FnCtlValue2
2289 ''' <summary>
2290 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2291 ''' </summary>
2292 ''' <param name="MCtlNo%"></param>
2293 ''' <remarks>
2294 ''' Date : 2022/04/28 �n��
2295 ''' </remarks>
2296 '''
2297 '''  1�F������       �{�P
2298 '''  2�F�g���n�j��   �{�P
2299 '''  3�F�g���m�f��   �{�P (���g�p)
2300 '''  4�F�z���G���[�� �{�P
2301 ''' 99�F�Ǐ��J�n�M�� OFF
2302 '''
2303 Function M% FnCtlValue2(ByVal MCtlNo%)
2304     FnCtlValue2 = 1
2305     Select MCtlNo%
2306         Case 1        '�������{�P
2307             M_Out(12569) = 0             '�����݊J�n�M��OFF
2308             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2309             MInputQty = M_In16(11600)    '��������M
2310             MInputQty = MInputQty + 1    '�������{�P
2311             M_Out16(12592) = MInputQty   '���������M
2312             M_Out(12569) = 1             '�����݊J�n�M��ON
2313             Break
2314             '
2315         Case 2        '�g���n�j���{�P
2316             M_Out(12569) = 0             '�����݊J�n�M��OFF
2317             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2318             MAssyOkQty = M_In16(11616)   '�g��OK����M
2319             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2320             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2321             M_Out(12569) = 1             '�����݊J�n�M��ON
2322             Break
2323             '
2324         Case 4        '�z���G���[���{�P
2325             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2326             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2327             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2328             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2329             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2330             M_Out(12569) = 1                       '�����݊J�n�M��ON
2331             Break
2332             '
2333         Case 99        '�Ǐ��J�n�M��OFF
2334             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2335             M_Out(12569) = 0        '�����݊J�n�M��OFF
2336             Break
2337             '
2338     End Select
2339     Exit Function
2340 FEnd
2341 '
2342 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2343 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2344 '-------------------------------------------------------------------------------
2345 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2346 '   ����
2347 '       PInspPos()      �F�����ʒu
2348 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2349 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2350 '       MInspCnt%       �F�����ʒu��
2351 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2352 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2353 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2354 '   �߂�l�F����
2355 '       0=�ُ�I���A1=����I��
2356 '
2357 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2358 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2359 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2360 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2361 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2362 '-------------------------------------------------------------------------------
2363     '----- �����ݒ� -----
2364     Cnt 0                                                           '�ړ�����������(�����l=0)
2365     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2366 '    Cnt 1,0.1,0.1
2367     '�ϐ��錾�E������
2368     Def Inte MNum                                                   '�����ԍ�(������1�`)
2369     MNum% = 1                                                       '�����ԍ������l�ݒ�
2370     Def Inte MEndFlg                                                '�����I���t���O
2371     MEndFlg% = 0
2372     '
2373     '����G�ԍ��ݒ�v���E�������s�v��off
2374     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2375     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2376     '�G���[�ԍ��N���A
2377     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2378     M_Out16(MOUT_InspErrNum) = MInspErrNum
2379     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2380     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2381     '
2382     'Insight Ready check?
2383     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2384         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2385         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2386         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2387         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2388         Exit Function
2389     EndIf
2390     '
2391     '�����ʒu���m�F
2392     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2393         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2394         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2395         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2396         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2397         Exit Function
2398     EndIf
2399     '
2400     '
2401     '
2402     '----- ���C������ -----
2403     '�ݒ肳�ꂽ�����ʒu�����̌������s
2404     While( MEndFlg% = 0 )
2405         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2406         MSetGrNumRetryExitFlg = 0
2407         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2408         While( MSetGrNumRetryExitFlg = 0 )
2409         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2410             '
2411             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2412             '
2413             '----- �����O���[�v�ԍ��ݒ� -----
2414             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2415             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2416             '
2417             '�����ʒu�ֈړ��E�ړ������҂�
2418             Mvs PInspPos( MNum% )                                       '�ړ�
2419             Dly 0.05                                                    '�ړ�������Delay
2420             '
2421             '�����O���[�v�ԍ��ݒ�I���m�F
2422             M_Timer(1) = 0
2423             MExitFlg = 0
2424             While( MExitFlg = 0 )
2425                 '����G�ݒ萳��I��?
2426                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2427                     MExitFlg = 1
2428                 '
2429                 '����G�ݒ�ُ�I��?
2430                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2431                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2432                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2433                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2434                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2435                     EndIf
2436                     MExitFlg = 1
2437                 '
2438                 'timeout�`�F�b�N
2439                 ElseIf 1000 < M_Timer(1) Then
2440                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2441                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2442                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2443                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2444                     EndIf
2445                     MExitFlg = 1
2446                 EndIf
2447             WEnd
2448             '
2449             '����G�ԍ��ݒ�v��off
2450             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2451             '
2452             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2453             'NG�Ȃ���Δ�����
2454             If MCurrentStepErr = 0 Then
2455                 MSetGrNumRetryExitFlg = 1
2456             Else
2457                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2458                 If MSetGrNumRetryCnt = 0 Then
2459                     MSetGrNumRetryExitFlg = 1
2460                 Else
2461                     'Retry�ց@���̑O��Delay
2462                     Dly 0.5
2463                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2464                 EndIf
2465             EndIf
2466             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2467             '
2468         WEnd
2469         '
2470         '
2471         '
2472         '----- �������s -----
2473         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2474             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2475                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2476                 MInspRetryExitFlg = 0
2477                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2478                 While( MInspRetryExitFlg = 0 )
2479                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2480                     '
2481                     '���������m�F
2482                     MRetryCnt = MRetryCnt - 1
2483                     M_Timer(1) = 0
2484                     MExitFlg = 0
2485                     While( MExitFlg = 0 )
2486                     '���������҂�
2487                         '����OK�I��?
2488                         If M_In( MIN_IS_InspOK% ) = 1  Then
2489                             MJudgeOKFlg = 1                         '����OK�t���OON
2490                             MExitFlg = 1
2491                         '
2492                         '����NG�I��?
2493                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2494                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2495                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2496                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2497                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2498                                 EndIf
2499                             EndIf
2500                             MExitFlg = 1
2501                         '
2502                         '�����ُ�I��(IS timeout)?
2503                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2504                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2505                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2506                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2507                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2508                                 EndIf
2509                             EndIf
2510                             MExitFlg = 1
2511                         '
2512                         'timeout�`�F�b�N
2513                         ElseIf 3000 < M_Timer(1) Then
2514                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2515                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2516                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2517                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2518                                 EndIf
2519                             EndIf
2520                             MExitFlg = 1
2521                         EndIf
2522                     WEnd
2523                     '
2524                     '�����J�n�v��off
2525                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2526                     '
2527                     'OK�Ȃ甲����
2528                     If MJudgeOKFlg = 1 Then
2529                         MInspRetryExitFlg = 1
2530                     Else
2531                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2532                         If MRetryCnt = 0 Then
2533                             MInspRetryExitFlg = 1
2534                         Else
2535                             'Retry�ց@���̑O��Delay
2536                             Dly 0.3
2537                         EndIf
2538                     EndIf
2539                     '
2540                 WEnd
2541             EndIf
2542         EndIf
2543         '
2544         '
2545         '
2546         MNum% = MNum% + 1                                           '����Step+1
2547         '�����I���m�F�@�����I���t���O�Z�b�g
2548         If (MInspCnt% < MNum% ) Then
2549             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2550         EndIf
2551         'NG���������s������
2552         If MInspErrNum <> 0 Then                                    'NG����?
2553             If MNgContinue% <> 1 Then                               'NG���s?
2554                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2555             EndIf
2556         EndIf
2557     WEnd
2558     '
2559     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2560     If 0 < MZAxis% Then
2561         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2562         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2563         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2564     EndIf
2565     '
2566     '�߂�l�ݒ�
2567     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
2568         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2569     Else
2570         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2571         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2572         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2573     EndIf
2574     '
2575 FEnd
2576 '
2577 ' ��ISInspection
2578 ''' <summary>
2579 ''' Insight�ɂ��摜�����������s
2580 ''' </summary>
2581 '''<param name="PInspPos()">�����ʒu</param>
2582 '''<param name="MInspGrNum%()">�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j</param>
2583 '''             PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2584 '''<param name="MInspCnt%">�����ʒu��</param>
2585 '''<param name="MZAxis%">�I������Z���ޔ����W�i-1:�����j</param>
2586 '''             �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2587 '''<param name="MNgContinue%">=1�Ō����G���[�ENG�������ɑSStep�̌������s��</param>
2588 '''<returns>    ���� 0=�ُ�I���A1=����I��</returns>
2589 '''         MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2590 '''         MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���"
2591 ''' <remarks>
2592 ''' Date   : 2021/07/07 : M.Hayakawa
2593 ''' </remarks>
2594 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2595 '    '�摜�g�p�m�F 0<- �摜�m�F�����̏ꍇ
2596 '    If M_In(11369) = 0 Then            'toRBT_�g�p�m�F
2597 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2598 '    EndIf
2599 ''
2600 '    Cnt 0                                                       '�ړ�����������(�����l=0)
2601 '    Fine 0.05,P                                                 '�ʒu���ߊ��������ݒu�@0.05mm
2602 '    MNum% = 1                                                   '�����ԍ������l�ݒ�
2603 '    Def Inte MEndFlg                                            '�����I���t���O
2604 '    MEndFlg% = 0
2605 '    '
2606 '    '�G���[�ԍ��N���A
2607 '    MInspErrNumSub = 0                                          '�������s�G���[�ԍ�sub
2608 '    MInspErrNum = 0                                             '�������s�G���[�ԍ�
2609 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2610 '    MInspNGStepNum = 0                                          '�������sNGStep�ԍ�
2611 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2612 '    '
2613 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready off�Ȃ�I��
2614 '        MInspErrNum = 20                                        '�������s�G���[�ԍ� 20 Insight offline
2615 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2616 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2617 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2618 ''
2619 '    EndIf
2620 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2621 '    '
2622 '    '�����ʒu���m�F
2623 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2624 '        MInspErrNum = 21                                        '�����f�[�^�Ȃ� 21�@����<1
2625 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2626 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2627 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2628 ''
2629 '    EndIf
2630 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2631 '    '
2632 '    '�ݒ肳�ꂽ�����ʒu�����̌������s
2633 '    While( MEndFlg% = 0 )
2634 '        '�����I���m�F�@�����I���t���O�Z�b�g
2635 '        If (MInspCnt% < MNum% ) Then
2636 '            MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2637 '        EndIf
2638 '        '
2639 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����J�n�@INSPTAST1
2640 '        If MEndFlg% = 0 Then
2641 '            M_01# = MInspGrNum%(MNum%)                          '����G�ԍ����n��
2642 '        EndIf
2643 '        M_02# = MEndFlg%                                        '�����I���t���O���n��
2644 '        M_05# = MNum%                                           '�����ԍ�(������1�`)
2645 '        '�^�X�N�@����G�ݒ�t���O���n��
2646 '        If MEndFlg% = 0 Then
2647 '            If 0 < MInspGrNum%(MNum%) Then
2648 '                M_03# = 1
2649 '            Else
2650 '                M_03# = 0
2651 '            EndIf
2652 '        Else
2653 '            M_03# = 0
2654 '        EndIf
2655 '        '�^�X�N�@�������ʊm�F�t���O���n��
2656 '        If 1 < MNum% Then
2657 '            If 0 < MInspGrNum%(MNum%-1) Then
2658 '                M_04# = 1
2659 '            Else
2660 '                M_04# = 0
2661 '            EndIf
2662 '        Else
2663 '            M_04# = 0
2664 '        EndIf
2665 '        '
2666 '        '�^�X�N�����J�n
2667 '        M_00# = 1                                               'TASK�����J�n
2668 '        '�^�X�N�����J�n�m�F
2669 '        M_Timer(1) = 0
2670 '        MExitFlg = 0
2671 '        While( MExitFlg = 0 )
2672 '            '�����J�n�����m�F
2673 '            If M_00# = 0 And M_10# = 8 Then
2674 '                MExitFlg = 1
2675 '            EndIf
2676 '            'timeout�`�F�b�N
2677 '            If 2000 < M_Timer(1) Then
2678 '                If MNgContinue% = 1 Then                        'NG���s?
2679 '                    MInspErrNumSub = 36                         '�G���[�ԍ��ݒ�36
2680 '                Else
2681 '                    MInspErrNum = 36                            '�G���[�ԍ��ݒ�36
2682 '                EndIf
2683 '                MExitFlg = 1
2684 '            EndIf
2685 '        WEnd
2686 '        '
2687 '        '�����ʒu�ֈړ��E�ړ������҂�
2688 '        If 0 = MInspErrNum Then
2689 '            If MEndFlg% = 0 Then
2690 '                Mvs PInspPos( MNum% )                           '�ړ�
2691 '            EndIf
2692 '        EndIf
2693 '        '
2694 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����I���҂��@INSPTAST1
2695 '        If 0 = MInspErrNum Then
2696 '            M_Timer(1) = 0
2697 '            MExitFlg = 0
2698 '            While( MExitFlg = 0 )
2699 '                '���������҂��i����I���j
2700 '                If M_10# = 1 Then
2701 '                    MExitFlg = 1
2702 '                EndIf
2703 '                '���������҂��i�ُ�I���j
2704 '                If M_10# = 0 Then
2705 '                    If MNgContinue% = 1 Then                    'NG���s?
2706 '                        MInspErrNumSub = M_12#                  '�G���[�ԍ��ݒ�@M12
2707 '                    Else
2708 '                        MInspErrNum = M_12#                     '�G���[�ԍ��ݒ�@M12
2709 '                    EndIf
2710 '                    MExitFlg = 1
2711 '                EndIf
2712 '                'timeout�`�F�b�N
2713 '                If 5000 < M_Timer(1) Then
2714 '                    If MNgContinue% = 1 Then                    'NG���s?
2715 '                        MInspErrNumSub = 31                     '�G���[�ԍ��ݒ�31
2716 '                    Else
2717 '                        MInspErrNum = 31                        '�G���[�ԍ��ݒ�31
2718 '                    EndIf
2719 '                    MExitFlg = 1
2720 '                EndIf
2721 '            WEnd
2722 '        EndIf
2723 '        '
2724 '        '�������ʊm�F
2725 '        If 0 = MInspErrNum Then
2726 '            If 1 < MNum% Then
2727 '                If 0 < MInspGrNum%(MNum%-1) Then                '��������?
2728 '                    If M_11# = 2 Then                           '����NG?
2729 '                        If MNgContinue% = 1 Then                'NG���s?
2730 '                            If MInspNGStepNum = 0 Then          'NG������?
2731 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2732 '                            EndIf
2733 '                            MInspErrNumSub = 32                 '�G���[�ԍ��ݒ� 32:����NG
2734 '                        Else
2735 ''                            MInspNGStepNum = MNum% - 1          '�������sNGStep�ԍ��ݒ�
2736 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2737 '                            MInspErrNum = 32                    '�G���[�ԍ��ݒ� 32:����NG
2738 '                        EndIf
2739 '                   EndIf
2740 '                EndIf
2741 '            EndIf
2742 '        EndIf
2743 '        '
2744 '        '�G���[�Ȃ猟�����f�I������̂�Loop���甲���邽�ߏI���t���O�Z�b�g
2745 '        If 0 <> MInspErrNum Then
2746 '            MEndFlg% = 1
2747 '        EndIf
2748 '        '
2749 '        '�������s�A�捞�����҂�
2750 '        If 0 = MInspErrNum Then
2751 '            If MEndFlg% = 0 Then
2752 '                If 0 < MInspGrNum%(MNum%) Then                  '��������?
2753 '                    M_Out(MOUT_IS_Insp%) = 1                    '�������s�v��on
2754 '                    '�捞�����m�F
2755 '                    M_Timer(1) = 0
2756 '                    MExitFlg = 0
2757 '                    While( MExitFlg = 0 )
2758 '                        '���������҂�
2759 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2760 '                            MExitFlg = 1
2761 '                        EndIf
2762 '                        'timeout�`�F�b�N
2763 '                        If 2000 < M_Timer(1) Then
2764 '                            If MNgContinue% = 1 Then            'NG���s?
2765 '                                MInspErrNumSub = 33             '�G���[�ԍ��ݒ�33
2766 '                            Else
2767 '                                MInspErrNum = 33                '�G���[�ԍ��ݒ�33
2768 '                            EndIf
2769 '                            MExitFlg = 1
2770 '                        EndIf
2771 '                    WEnd
2772 '                EndIf
2773 '                '
2774 '            EndIf
2775 '        EndIf
2776 '        MNum% = MNum% + 1
2777 '    WEnd
2778 '    '
2779 '    '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2780 '    If 0 < MZAxis% Then
2781 '        PCurrentPos = P_Curr                                    '���݈ʒu�擾
2782 '        PCurrentPos.Z = MZAxis%                                 'Z����ݒ�
2783 '        Mvs PCurrentPos                                         '���݈ʒu���ֈړ�
2784 '    EndIf
2785 '    '
2786 '    'NG���s������
2787 '    If MNgContinue% = 1 Then                                    'NG���s?
2788 '        MInspErrNum = MInspErrNumSub                            '�G���[�ԍ��ݒ�
2789 '    EndIf
2790 '    '
2791 '    '�߂�l�ݒ�
2792 '    If MInspErrNum = 0 Then
2793 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2794 '    Else
2795 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2796 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2797 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2798 '    EndIf
2799 '    '
2800 '*ISInspection_End
2801 'FEnd
2802 '
2803 '��InitialZoneB
2804 ''' <summary>
2805 ''' ����~��̕��A����
2806 ''' 1)���ޔ��@Z������Ɉړ�
2807 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
2808 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
2809 ''' 4)�C�j�V�����|�W�V�����ֈړ�
2810 ''' </summary>
2811 ''' <remarks>
2812 ''' Date : 2022/04/11 : N.Watanabe
2813 ''' </remarks>
2814 Function V fnInitialZoneB()
2815     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/04/26 �n��
2816 '
2817 '�p�����[�^
2818     Ovrd 5
2819 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2820 '    Cmp Pos, &B100011
2821 '
2822 '���A����J�n
2823 '
2824 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������
2825 *RecoveryChuckOpen
2826     PActive = P_Curr          '���݈ʒu���擾
2827     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
2828 'PMechaOnRoboSet(DVD���J�u����)�́A�`���b�N���
2829     If (PActive.X <= PMechaOnRoboSet.X + 1.0) And (PActive.X >= PMechaOnRoboSet.X -1.0) Then
2830         If (PActive.Y <= PMechaOnRoboSet.Y + 1.0) And (PActive.Y >= PMechaOnRoboSet.Y -1.0) Then
2831             If (PActive.Z <= PMechaOnRoboSet.Z + 1.0) And (PActive.Z >= PMechaOnRoboSet.Z -1.0) Then
2832                 MRecoveryChuckOpen = 1
2833             EndIf
2834         EndIf
2835     EndIf
2836 'PMechaOnRoboGet(DVD���J�󂯎��ʒu)�́A�`���b�N���
2837     If (PActive.X <= PMechaOnRoboGet.X + 1.0) And (PActive.X >= PMechaOnRoboGet.X -1.0) Then
2838         If (PActive.Y <= PMechaOnRoboGet.Y + 1.0) And (PActive.Y >= PMechaOnRoboGet.Y -1.0) Then
2839             If (PActive.Z <= PMechaOnRoboGet.Z + 1.0) And (PActive.Z >= PMechaOnRoboGet.Z -1.0) Then
2840                 MRecoveryChuckOpen = 1
2841             EndIf
2842         EndIf
2843     EndIf
2844 '
2845 '    If MRecoveryChuckOpen = 1 Then
2846 '        M_Out(12256) = 0        'DVD�`���b�N��OFF
2847 '        M_Out(12257) = 1        'DVD�`���b�N�JON
2848 '        M_20# = 0               'KEY���͏�����
2849 '        MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
2850 '        If MRtn = 0 Then
2851 '            fErrorProcess(11,270,284,0)
2852 '            If M_20# = MNext% Then M_20# = MClear%
2853 '            If M_20# = MAbout% Then GoTo *RecoveryEnd
2854 '            If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2855 '            If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2856 '        Else
2857 '            M_Out(12257) = 0        'DVD�`���b�N�JOFF
2858 '        EndIf
2859 '    EndIf
2860 '
2861     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2862     M_Out(12256) = 0                           'DVD�`���b�N��OFF
2863     M_Out(12257) = 1                           'DVD�`���b�N�JON
2864 '
2865     M_20# = 0                                  'KEY���͏�����
2866     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
2867     If MRtn = 1 Then M_Out(12257) = 0          'DVD�`���b�N�JOFF
2868     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2869 '
2870     fErrorProcess(11,270,284,0)
2871     If M_20# = MNext% Then M_20# = MClear%
2872     If M_20# = MAbout% Then GoTo *RecoveryEnd
2873     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2874     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2875 '
2876     *RecoveryChuckOpenEnd
2877 '
2878 '�������@���ځA���ޔ����o���Ȃ����̑Ώ�
2879 '
2880 'PMechaOnRoboSet(Get)�`PMechaOnRoboSet(Get)_1�̃G���A�ɂ���Ƃ��́APMechaOnRoboSet_1��
2881 '�EPMechaOnRoboSet
2882 '�EPMechaOnRoboSet_1
2883 '�EPMechaOnRoboGet
2884 '�EPMechaOnRoboGet_1
2885 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2886     PActive = P_Curr                    '���݈ʒu���擾
2887     If (PActive.X >= -150) And (PActive.X <= -60) Then
2888         If (PActive.Y >= 540) And (PActive.Y <= 580) Then
2889             If (PActive.Z >= 290) And (PActive.Z <= 330) Then
2890                 Mvs PMechaOnRoboSet_1
2891                 Dly 1.0
2892             EndIf
2893         EndIf
2894     EndIf
2895 '
2896 'PMechaOnRoboSet(Get)_1�`PMechaOnRoboSet(Get)_2�̃G���A�ɂ���Ƃ��́APMechaOnRoboSet_2��
2897 '�EPMechaOnRoboSet_1
2898 '�EPMechaOnRoboSet_2
2899 '�EPMechaOnRoboGet_1
2900 '�EPMechaOnRoboGet_2
2901 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2902 '    PActive = P_Curr                    '���݈ʒu���擾
2903 '    If (PActive.X >= -90) And (PActive.X <= 50) Then
2904 '        If (PActive.Y >= 300) And (PActive.Y <= 570) Then
2905 '            If (PActive.Z >= 290) And (PActive.Z <= 430) Then
2906 '                Mvs PMechaOnRoboSet_2
2907 '                Dly 1.0
2908 '            EndIf
2909 '        EndIf
2910 '    EndIf
2911 '
2912 '���ޔ�
2913     PActive = P_Curr
2914     Pmove = PActive
2915     Pmove.Z = 500           '���ޔ�����ꗥ�̍���
2916      If PActive.X < -400 Then
2917         Pmove.Z =290        '����(F)�󂯎��ʒu��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
2918     EndIf
2919     If PActive.X > 400 Then
2920         Pmove.Z =400        '�p���b�g��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
2921     EndIf
2922     If PActive.Z < Pmove.Z Then
2923         Mvs Pmove
2924     EndIf
2925     Dly 1.0
2926 'J1���ȊO��ޔ��|�W�V�����ֈړ�
2927     JActive = J_Curr
2928     Jmove = JTaihi
2929     Jmove.J1 = JActive.J1        'J1���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
2930     Jmove.J6 = JActive.J6        'J6���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
2931     Mov Jmove
2932     Dly 1.0
2933 'J1���݂̂�ޔ��|�W�V�����ֈړ�
2934     Mov JTaihi
2935     Dly 1.0
2936 '�C�j�V�����|�W�V�����ֈړ�
2937     Mov PInitialPosition
2938     Cmp Off
2939     Ovrd 100
2940 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n         '2022/04/20 �t�@���N�V�����̊O�ֈړ� �n��
2941 '    If M_In(11856) = 0 Then                 ' ��~���̂�
2942 '        M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
2943 '        MRet = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
2944 '        If MRet = 0 Then
2945 '        Else
2946 '            M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
2947 '        EndIf
2948 '    EndIf
2949     fErrorProcess(11,253,281,0)
2950 *RecoveryEnd
2951     Exit Function
2952 FEnd
2953 '
2954 '
2955 '��fnAutoScreenComment
2956 ''' <summary>
2957 ''' ���C����ʂ̓���󋵕\��
2958 ''' �R�����gD1005�̐ݒ�
2959 ''' </summary>
2960 '''<param name="McommentD1005%">�R�����gID</param>
2961 ''' <remarks>
2962 ''' Date   : 2021/07/07 : M.Hayakawa
2963 ''' </remarks>
2964 Function fnAutoScreenComment(ByVal McommentD1005%)
2965     M_Out16(12576) = McommentD1005%
2966 FEnd
2967 '
2968 '��fnRoboPosChk
2969 ''' <summary>
2970 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2971 ''' </summary>
2972 '''<param name="MINNumber%">���͔ԍ�</param>
2973 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2974 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2975 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2976 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2977 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2978 ''' <remarks>
2979 ''' Date   : 2021/07/07 : M.Hayakawa
2980 ''' </remarks>
2981 Function M% fnRoboPosChk
2982     fnRoboPosChk = 0
2983     MRet = fnStepRead()
2984     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2985     '�E�B���h��ʐ؊���
2986     If MRBTOpeGroupNo > 5 Then
2987         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2988         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2989         Dly 0.2
2990         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2991         Dly 1.5
2992         '
2993         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2994         '
2995         MLoopFlg% = 1
2996         While MLoopFlg% = 1
2997             '
2998             '
2999             MKeyNumber% = fnKEY_WAIT()
3000             Select MKeyNumber%
3001                 Case Is = MAbout%       '��~
3002                     M_20# = MAbout%
3003                     MLoopFlg% = -1
3004                     Break
3005                 Case Is = MNext%        '����
3006                     'MLoopFlg% = -1
3007                     Break
3008                 Case Is = MContinue%    '�p��
3009                     M_20# = MContinue%
3010                     MLoopFlg% = -1
3011                     Break
3012                 Default
3013                     Break
3014             End Select
3015         WEnd
3016     EndIf
3017     '
3018     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
3019         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3020         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
3021         Select MRBTOpeGroupNo
3022             Case Is = 5                          '�������Ȃ�
3023                 Break
3024             Case Is = 10                         '�����ʒu�֖߂�
3025                 'Mov PTEST001
3026                 Break
3027             Case Is = 15                         '�����ʒu�֖߂�
3028                 'Mov PTEST002
3029                 Dly 0.5
3030                 'Mov PTEST001
3031                 Dly 0.5
3032                 Break
3033             Default
3034                 Break
3035         End Select
3036         '
3037         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
3038         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
3039         MRBTOpeGroupNo = 5
3040         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
3041         Dly 1.0
3042         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
3043         fnRoboPosChk = 1                        '�����ʒu������s
3044         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3045     EndIf
3046     Exit Function
3047 FEnd
3048 '
3049 '��frInCheck
3050 ''' <summary>
3051 ''' �Z���T�[IN�`�F�b�N
3052 ''' </summary>
3053 '''<param name="MINNumber%">���͔ԍ�</param>
3054 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3055 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3056 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3057 ''' <remarks>
3058 ''' Date   : 2021/07/07 : M.Hayakawa
3059 ''' </remarks>
3060 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3061     M_Timer(4) = 0
3062     MloopFlg = 0
3063     While MloopFlg = 0
3064         MCrtTime& = M_Timer(4)
3065         If M_In(MINNumber%) = MCMPFLG% Then
3066             MloopFlg = 1
3067             frInCheck = 1
3068         ElseIf MCrtTime& > MTimeCnt& Then
3069             MloopFlg = 1
3070             frInCheck = 0
3071         EndIf
3072     WEnd
3073 FEnd
3074 '-----------------------------------------------
3075 '
3076 '�˂����ߋ@�ʐM�m�F
3077 '
3078 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3079 'fScewTcomChk = 0�@�F����I��
3080 '          �@�@ -1 �F�ُ�I��
3081 '-----------------------------------------------
3082 Function M% fScewTcomChk
3083 *ReCheckScewTcomChk
3084     fScewTcomChk = 0
3085     '�ʐM�m�F���M
3086     M_Out(MOUT_ScwT_ComChk%) = MOn%
3087     '�ʐM�m�F��M�ҋ@
3088 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3089     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3090     '�ʐM�m�F���M�I��
3091     M_Out(MOUT_ScwT_ComChk%) = MOff%
3092     If MRtn = 0 Then
3093         fScewTcomChk = -1
3094     EndIf
3095     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
3096  '
3097 FEnd
3098 '
3099 '
3100 '-----------------------------------------------
3101 '
3102 '�˂����ߊJ�n���M
3103 '
3104 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3105 'fScewTStart = 0�@�F����I��
3106 '          �@�@-1 �F�ُ�I��
3107 '-----------------------------------------------
3108 Function M% fScewTStart
3109     fScewTStart = 0
3110     nRet% = 0
3111     '�˂����ߊJ�n�ҋ@����M
3112 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3113     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3114     If MRtn = 0 Then nRet% = -1
3115     If MRtn = 0 Then GoTo *ScrewStartERROR      '�J�n�ł��Ȃ������ꍇ�W�����v
3116     Dly 0.1
3117     '�˂����ߊJ�n��M�𑗐M
3118     M_Out(MOUT_ScwT_ST%) = MOn%
3119     Dly 0.5
3120     'Wait M_In(MTEST_KEY%) = MOn%
3121     '�˂����ߊJ�n���M�I��
3122     M_Out(MOUT_ScwT_ST%) = MOff%
3123     '
3124 *ScrewStartERROR
3125     fScewTStart = nRet%
3126 FEnd
3127 '
3128 '
3129 '
3130 '-----------------------------------------------
3131 '
3132 '�˂����ߊ�����M
3133 '
3134 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3135 'fScewTcomChk = 0�@�F����I��
3136 '          �@ �@-1 �F�ُ�I��
3137 '-----------------------------------------------
3138 Function M% fScewTFinish
3139 *ReCheckScewTFinish
3140     fScewTFinish = 0
3141     '�˂����ߊ����ҋ@����M
3142 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3143     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3144     If MRtn = 0 Then
3145         fScewTFinish = -1
3146     EndIf
3147     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3148     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3149     Dly 0.1
3150     '�˂����ߊ�����M�𑗐M
3151     M_Out(MOUT_ScwT_FinOK%) = MOn%
3152     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3153     '�˂����ߊJ�n���M�I��
3154     M_Out(MOUT_ScwT_FinOK%) = MOff%
3155     'Wait M_In(MTEST_KEY%) = MOn%
3156     '
3157 *ScewTFinish_ErrEnd
3158 FEnd
3159 '
3160 '
3161 '-----------------------------------------------
3162 '
3163 '����xx��~��M
3164 '
3165 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3166 'fScewTCaseStop = 0�@�F����I��
3167 '          �@   �@-1 �F�ُ�I��
3168 '-----------------------------------------------
3169 Function M% fScewTCaseStop(ByVal MCase%())
3170 *ReCheckScewTCaseStop
3171     fScewTCaseStop = 0
3172     '����xx��~����M
3173     Wait M_In(MCase%(1)) = MOn%
3174     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3175     If MRtn = 0 Then
3176         fScewTCaseStop = -1
3177     EndIf
3178     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3179     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3180     Dly 0.1
3181     '����xx��~��M�𑗐M
3182     M_Out(MCase%(2)) = MOn%
3183     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3184     '�˂����ߊJ�n���M�I��
3185     M_Out(MCase%(2)) = MOff%
3186 *ScewTCaseStop_ErrEnd
3187     '
3188 FEnd
3189 ''
3190 '��fScrewTighenRoboCheck
3191 '<summary>
3192 '�˂����{�Ď�
3193 '</summary>
3194 '<param name = "MStopNum%"> ��~�ԍ�</param>
3195 '<returns>���� 0:�˂����{�ُ�I�� 1:OK </returns>
3196 '<make>
3197 '2021/12/2 �����V��
3198 '</make>
3199 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3200     fnAutoScreenComment(503)    '��ԕ\��[�˂����{����I���҂�] 2022/04/26 �n��
3201     fScrewTighenRoboCheck = 1
3202     MScrewTighenRoboFlg% = 1    '�t���O�̏�����
3203     MCheck% = 0
3204     While MScrewTighenRoboFlg% = 1
3205         MCheck% = M_In16(11904)
3206         If M_In(MStopNum%) = 1 Then     '��~�ʒu�܂ŗ�����
3207             MScrewTighenRoboFlg% = 0    '�֐��𔲂���
3208             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
3209         EndIf
3210         If MCheck% <> 0 Then
3211             fScrewTighenRoboError(MCheck%)
3212             Select M_20#
3213                 Case MAbout%            '��~�������ꂽ�ꍇ
3214                     M_Out(12869) = 1 Dly 1.0
3215                     MScrewTighenRoboFlg% = 0
3216                     fScrewTighenRoboCheck = 0   '�ُ�I��
3217                     Break
3218                 Case MNgProcess%        'NG�������ꂽ�ꍇ
3219                     M_Out(12873) = 1 Dly 1.0
3220                     MScrewTighenRoboFlg% = 0
3221                     fScrewTighenRoboCheck = 0   '�ُ�I��
3222                     Break
3223                 Case MContinue%             '���g���C�������ꂽ�ꍇ
3224                     M_20# = MClear%         'M_20#������
3225                     M_Out(12871) = 1 Dly 1.0
3226                     Break
3227                 Case MNext%                 '���ւ������ꂽ�ꍇ
3228                     M_20# = MClear%         'M_20#������
3229                     M_Out(12874) = 1 Dly 1.0
3230                     Break
3231             End Select
3232             Dly 0.5
3233         EndIf
3234     WEnd
3235 FEnd
3236 '��fScrewTighenRoboError
3237 '<summary>
3238 '�˂����{�G���[����
3239 '</summary>
3240 '<param name = "ErrorCode%"> �G���[�ԍ�</param>
3241 '<make>
3242 '2021/12/2 �����V��
3243 '</make>
3244 Function fScrewTighenRoboError(ErrorCode%)
3245     MCommentD1001 = ErrorCode% + 300
3246     fErrorProcess(11,MCommentD1001,0,0)
3247 FEnd
3248 '
3249 '��fErrorProcess
3250 '<summary>
3251 '�G���[����
3252 '</summary>
3253 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
3254 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
3255 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
3256 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
3257 '<make>
3258 '2021/11/5 �����V��
3259 '</make>
3260 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3261     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
3262     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
3263     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
3264     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
3265     MKeyNum% = 0
3266 *RETRY_ERR_PROCESS
3267      M_20# = MClear%     '������
3268 '        '�G���[�����L�q
3269         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3270 '        'GOT KEY���͑҂�
3271         MKeyNum% = fnKEY_WAIT()
3272 '        '
3273         If MKeyNum% = MAbout% Then   '��~��I�������ꍇ
3274             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3275             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3276             Break
3277          '
3278         ElseIf MKeyNum% = MContinue% Then   '�p����I�������ꍇ
3279             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3280             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3281         '
3282         ElseIf MKeyNum% = MNext% Then   '���ւ�I�������ꍇ
3283             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3284             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3285          '
3286         ElseIf MKeyNum% = MNgProcess% Then   '��~��I�������ꍇ
3287             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3288             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3289             Break
3290         '
3291         EndIf
3292         '
3293         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3294 FEnd
3295 '
3296 '��fnTorqueCheck
3297 ''' <summary>
3298 ''' �g���N�`�F�b�N����p�̃��C��
3299 ''' </summary>
3300 ''' <remarks>
3301 ''' Date   : 2021/12/21 : H.AJI
3302 ''' </remarks>'
3303 Function M% fnTorqueCheck
3304     '�g���N�`�F�b�N�����M  �����n��~
3305     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3306     '
3307     fnTorqueCheck = 0
3308     Ovrd 20
3309     Mov PInitialPosition              '�����ʒu�ړ�
3310     Accel 100 , 20
3311     Mvs PHandChange                   '�n���h�����ʒu
3312     Accel 100 , 100
3313     Ovrd 100
3314     Ovrd 100
3315     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3316     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3317     Dly 0.2
3318     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3319     '
3320     'M6340  �g���N�`�F�b�N��M
3321     'Dly 5.0
3322     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3323     Dly 1.0
3324     M_Out(12340) = 0
3325     '
3326     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3327     M_Out(12835) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3328    Wait M_In(11843) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3329     M_Out(12835) = 0                         '�˂����{�g���N�`�F�b�N��ʐؑ֊���
3330     '
3331     '
3332     MLoopFlg = 1
3333     While MLoopFlg = 1
3334         '
3335 '        Mov PInitialPosition              '�����ʒu�ړ�
3336         '
3337         MKeyNumber = fnKEY_WAIT()
3338         Select MKeyNumber
3339             Case Is = 1           '��~
3340                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3341                 Dly 1.0
3342                 M_Out(12343) = 0
3343                 Ovrd 20
3344                 'Mov PTicketRead_1
3345                 M_Out(12840) = 1          '�g���N�`�F�b�N�I��
3346                 Wait M_In(11859) = 1      '�˂����{����̏I��
3347                 M_Out(12840) = 0          '�g���N�`�F�b�N�I��
3348                 Ovrd 100
3349                 M_20# = 1
3350                 MLoopFlg = -1
3351                 Break
3352             Case Is = 2           '����
3353                 Break
3354             Case Is = 3           '�p��
3355                 Break
3356             Case Is = 4           '�g���N�`�F�b�N�J�n
3357                 M_Out(12342) = 1          '�g���N�`�F�b�N�J�n�v����M M6342
3358                 Dly 1.0
3359                 M_Out(12342) = 0
3360                 If M_In(11862) = 1 Then             '�g���N�`�F�b�J�[�m�F
3361                     fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3362                     MRet = fnScrewMTorque()           '�˂����{�p�g���N�`�F�b�N
3363                 EndIf
3364                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3365                 'MRet = fnMoveTorquePosi()
3366                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3367                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3368                 Break
3369             Default
3370                 Break
3371         End Select
3372     WEnd
3373     '
3374     '�g���N�`�F�b�N����~���M
3375     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3376     '
3377     '���{�b�g�̈ʒu�����ɖ߂�
3378     Mvs PInitialPosition              '�C�j�V�����|�W�V����
3379     '
3380     '
3381  FEnd
3382  '
3383 '
3384 '
3385 '---------------------------
3386 '
3387 '    ���C����ʂ̕\���A��\���ݒ�
3388 '         �R�����gD1001, D1002, D1003�̐ݒ�
3389 '           MWindReSet = 0     ��ʔ�\��
3390 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3391 '           MWindErrScr = 10    �G���[��� D1001, D1002
3392 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3393 '
3394 '---------------------------
3395 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3396     fnMainScreenOpen = 0
3397     '
3398    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3399         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3400     EndIf
3401     '
3402     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3403         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3404     EndIf
3405     '
3406     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3407         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3408     EndIf
3409     '
3410     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3411     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3412     Dly 0.5
3413     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3414 FEnd
3415 '
3416 '��Main
3417 ''' <summary>
3418 ''' �g���N�`�F�b�N������
3419 ''' </summary>
3420 ''' <remarks>
3421 ''' Date   : 2021/12/21 : H.AJI
3422 ''' </remarks>'
3423 Function M% fnScrewMTorque
3424     fnScrewMTorque = 0
3425     M_Out(12838) = 1                         '�g���N�`�F�b�N�J�n1
3426     Wait M_In(11857) = 1                     '��M����
3427     M_Out(12838) = 0                         '�g���N�`�F�b�N�J�n1
3428     Dly 2.0
3429 FEnd
3430 '
3431 '
3432 '----------------------------------------------------------------
3433 'fTimeOutJudge
3434 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3435 '����
3436 'Address% = �Ď��A�h���X�ԍ�
3437 'JudgeFlg% = �ΏۃA�h���X�̐���I�����̒l
3438 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3439 '�߂�l = 0 �G���[
3440 '         1 ����I��
3441 '         2 ���g���C
3442 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3443 '�쐬��
3444 '2022/9/20 ����
3445 '----------------------------------------------------------------
3446 '
3447 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3448     fTimeOutJudge = 0
3449     MJudge% = 1
3450     MRtn = 0
3451     M_20# = MClear%
3452     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3453 *TimeOutLoop
3454     If MRtn = 1 Then GoTo *TimeOut
3455         fErrorProcess(11,202,203,0)
3456         If M_20# = MNext% Then GoTo *TimeOutLoop
3457         If M_20# = MContinue% Then MJudge% = 2
3458         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3459 *TimeOut
3460     fTimeOutJudge = MJudge%
3461 '
3462 *JUDGE_ERROR_END
3463 Exit Function
3464 FEnd
3465 '
3466 ''��Main
3467 ''' <summary>
3468 ''' �g������p�̃��C��
3469 ''' </summary>
3470 ''' <remarks>
3471 ''' Date   : 2021/07/07 : M.Hayakawa
3472 ''' </remarks>'
3473 Function Main
3474     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3475     '
3476     If M_Svo=0 Then
3477         Servo On
3478     EndIf
3479     Wait M_Svo=1
3480 '�g���X�^�[�g���t�����v���p���XON (�ʃX���b�g��8����v���ɕύX�j
3481 '    M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3482 '�p�g���C�g����
3483     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3484     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3485     '
3486     M_20# = 0                                   'KEY���͏�����
3487     'M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����(�ʒu�ړ�1/18����)
3488     MRet% = 0
3489 '�����ʒu�̊m�F�ƈړ�
3490 '
3491 '���A����@���s�E�����s����      2022/04/11 �n�� �쐬
3492     PActive = P_Curr                    '���݈ʒu���擾
3493     MRecoveryPass% = 0
3494     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3495         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3496             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3497                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3498             EndIf
3499         EndIf
3500     EndIf
3501     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3502         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3503             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3504                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3505             EndIf
3506         EndIf
3507     EndIf
3508     If (PActive.X <= PMechaOnJigGet_3.X + 1.0) And (PActive.X >= PMechaOnJigGet_3.X -1.0) Then
3509         If (PActive.Y <= PMechaOnJigGet_3.Y + 1.0) And (PActive.Y >= PMechaOnJigGet_3.Y -1.0) Then
3510             If (PActive.Z <= PMechaOnJigGet_3.Z + 1.0) And (PActive.Z >= PMechaOnJigGet_3.Z -1.0) Then
3511                 MRecoveryPass% = 1       'DVD���ʒu���ʒu�͕��A����p�X
3512             EndIf
3513         EndIf
3514     EndIf
3515     If MRecoveryPass% = 0 Then
3516        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3517     EndIf
3518 '
3519 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n        'fnInitialZoneB�̊O�ֈړ� 2022/04/20 �n��
3520     If MopeNo <> 2 And M_In(MIN_TorqueCheck%) <> 1 Then       '�g���N�`�F�b�N�̎��͈ȉ������s���Ȃ� 2022/04/21 �n��
3521         If M_In(11856) = 0 Then                 ' ��~���̂�
3522             fnAutoScreenComment(501)            ' ��ԕ\��[�l�W���ߋ@�����^�]�J�n��] 2022/04/25 �n��
3523             M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
3524             MRet% = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
3525             If MRet% = 0 Then
3526             Else
3527                 M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
3528             EndIf
3529         EndIf
3530     EndIf
3531 '
3532 '
3533 '    MRet% = fnRoboPosChk()
3534 '    If MRet% = 1 Then                           '�����ʒu�̓�����s�����ꍇ
3535 '        fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  '�E�B���h���
3536 '        MKeyNumber% = fnKEY_WAIT()
3537 '        Select MKeyNumber%
3538 '            Case Is = MAbout%       '��~
3539 '                M_20# = MAbout%
3540 '                MLoopFlg% = -1
3541 '                Break
3542 '            Case Is = MNext%        '����
3543 '                'MLoopFlg = -1
3544 '                Break
3545 '            Case Is = MContinue%    '�p��
3546 '                M_20# = MContinue%
3547 '                MLoopFlg% = -1
3548 '                Break
3549 '            Default
3550 '                Break
3551 '        End Select
3552 '    EndIf
3553     '
3554     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3555         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3556 '�g���N�`�F�b�N
3557         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3558             MRet% = fnTorqueCheck()
3559             Break
3560         Else
3561 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
3562 '                MRtn = InspInit()               '�摜��������������
3563 '            EndIf
3564             '
3565            M_20# = MClear%                    '������
3566 '�g���J�n
3567             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3568                 'MRet% = fnAssyStart()  '�b��R�����g�A�E�g
3569                 fnAssyStart()
3570             Else
3571                 M_20# = MPass%
3572             EndIf
3573             M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����(�ʒu�ړ�1/18����)
3574 '�g���I�����t����
3575             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3576             Wait M_In(11572) = 1            '���t�擾����
3577             Dly 0.1
3578             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3579 '���t�^�[���j�b�g�ւ�OUT
3580             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3581             fnAutoScreenComment(89)         'AUTO��� �g����������
3582             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3583 'OK/NG�t���O�o��
3584             If M_20# = MAssyOK% Or M_22# = MIrregular% Then
3585                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3586             ElseIf M_20# = MPass% Then
3587                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3588             EndIf
3589 'PIAS�ɑg������������
3590             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3591                 If M_20# = MPass% And M_22# <> MIrregular% Then
3592                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3593                 Else
3594                     'KEY���͂�NG�̏ꍇ
3595                     If M_20# = MNgProcess% Then
3596                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3597                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3598                         MRet% = fnPiasWrite(MNG%)
3599                        nAssyNgQty = nAssyNgQty + 1
3600                     EndIf
3601                     '
3602                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(0����MAssyOK%�ɕύX1/12����)
3603                     If M_20# = MAssyOK% Then
3604                             '-----------------------
3605                             'D732 -> D2600 �R�s�[�v��
3606                             M_Out(12566) = 1
3607 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3608                             M_Out(12566) = 0
3609                             '
3610                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3611                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3612                             '��ԍ��ƍ�(PP�͖��g�p�j
3613 '                            MRet% = fnPCBNumberCheck()
3614                         Else
3615                             MRet% = 1
3616                         EndIf
3617                         '
3618                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3619                             If M_20# <> MAbout% Then
3620                                 '�H������OK��������
3621                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3622                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3623                                 MRet% = fnPiasWrite(MOK%)
3624                                 nAssyOkQty = 0
3625                                 nAssyOkQty = nAssyOkQty + 1
3626                             Else
3627                                 nAssyOkQty = nAssyOkQty + 1
3628                             EndIf
3629                         EndIf
3630                     EndIf
3631 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3632 '                    MRet% = fnPiasWrite(MOK%)
3633                 EndIf
3634             Else
3635                 nAssyOkQty = nAssyOkQty + 1
3636             EndIf
3637             '
3638             '�g���I�����t��������
3639             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3640             '�������A�g��OK���A�g��NG��������
3641 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3642             '
3643 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3644 '                '�摜�����I������
3645 '                MRtn = InspQuit()
3646 '            EndIf
3647         EndIf
3648         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3649     EndIf
3650 '�p�g���C�g����
3651     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3652     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3653 'GOT�\��
3654     fnAutoScreenComment(93)  'AUTO��� �H������
3655 FEnd
3656 End
3657 '
3658 '
3659 '���܂��Ȃ��R�����g
3660 '��΍폜�����
3661 '
3662 '
3663 '
3664 '
3665 '
PInspPosition(1)=(+600.00,-152.00,+403.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
PInspPosition(2)=(+6.08,+550.00,+537.59,+145.00,+0.00,-90.00,+0.00,+0.00)(7,0)
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
PTemp=(+160.73,+268.78,+438.32,-105.54,+88.56,-15.42,+0.00,+0.00)(6,0)
PScrewPos(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(1)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(2)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PEscapePosi(10)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PActive=(+160.73,+268.78,+438.32,-105.54,+88.56,-15.42)(6,0)
Pmove=(+309.64,-1.37,+500.00,-179.87,+67.95,+179.97)(6,0)
PBracketFCheck=(-21.45,+489.13,+480.00,-180.00,+0.00,+0.00)(7,0)
PBracketFCheck1=(+2.27,+435.02,+540.18,+145.00,-0.01,-90.00)(7,0)
PBracketFCheck2=(+2.27,+535.02,+540.18,+145.00,-0.01,-90.00)(7,0)
PBracketFCheck_2=(-171.46,+546.61,+480.00,-180.00,+0.00,-90.00)(7,0)
PBracketFCheck_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet=(-595.13,-91.69,+243.56,-179.81,+0.51,+91.79)(7,1)
PBracketFGet_1=(-595.13,-91.96,+280.00,-179.81,+0.51,+91.79)(7,1)
PBracketFGet_2=(-287.41,+0.11,+490.00,-180.00,+0.00,+0.00)(7,0)
PBracketFGet_3=(-177.20,+193.83,+460.54,-179.99,-0.01,-85.26)(7,0)
PBracketFGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet=(-198.12,+546.19,+453.88,-179.44,+0.16,-88.98)(7,0)
PBracketFSet_1=(-198.12,+546.19,+470.00,-179.44,-0.16,-88.98)(7,0)
PBracketFSet_2=(-197.04,+546.94,+540.00,-179.88,-0.07,-88.98)(7,0)
PBracketFSet_3=(-26.65,+244.35,+540.00,-179.99,-0.01,-85.26)(7,0)
PBracketFSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck=(-177.56,+416.30,+479.98,-180.00,+0.00,-89.99)(7,0)
PBracketRCheck1=(+6.08,+420.00,+537.59,+145.00,+0.00,-90.00)(7,0)
PBracketRCheck2=(+6.08,+550.00,+537.59,+145.00,+0.00,-90.00)(7,0)
PBracketRCheck_2=(-177.57,+546.61,+479.97,-180.00,+0.00,-90.00)(7,0)
PBracketRCheck_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet=(-538.13,-90.29,+244.19,-179.51,+0.39,+92.03)(7,1)
PBracketRGet_1=(-538.13,-90.29,+290.00,-179.51,+0.39,+92.03)(7,1)
PBracketRGet_2=(-287.41,+0.11,+490.00,-180.00,+0.00,+0.00)(7,0)
PBracketRGet_3=(-177.20,+193.83,+460.54,-179.99,-0.01,-85.26)(7,0)
PBracketRGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet=(-155.13,+545.39,+453.83,+179.76,+0.30,-89.39)(7,0)
PBracketRSet_1=(-155.13,+545.39,+460.00,+179.76,+0.30,-89.39)(7,0)
PBracketRSet_2=(-155.13,+545.39,+520.00,+179.76,+0.30,-89.39)(7,0)
PBracketRSet_3=(-26.65,+244.35,+539.98,-179.99,-0.01,-85.26)(7,0)
PBracketRSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PHandChange=(+347.78,-1.40,+382.90,-90.00,+89.23,-89.99)(6,0)
PInitialPosition=(+303.63,-1.43,+467.78,+165.41,+90.00,+165.42)(6,0)
PInitialPosition1=(+302.91,+7.16,+469.96,-180.00,+0.00,+180.00)(7,0)
PMechaOnJigGet1=(+163.54,+364.18,+175.90,-99.82,+88.63,-9.71)(6,0)
PMechaOnJigGet1_1=(+163.54,+363.18,+200.00,-99.82,+88.63,-9.71)(6,0)
PMechaOnJigGet2=(+162.55,+364.22,+175.52,-89.65,+88.72,+0.00)(6,0)
PMechaOnJigGet2_1=(+162.55,+364.22,+200.00,-89.65,+89.72,+0.00)(6,0)
PMechaOnJigGet_2=(+160.73,+362.85,+350.00,-105.22,+88.56,-15.10)(6,0)
PMechaOnJigGet_3=(+160.73,+268.78,+438.32,-105.54,+88.56,-15.42)(6,0)
PMechaOnJigGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnJigGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnJigGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet=(+315.15,+116.04,+239.94,-29.59,+88.55,-30.45)(6,0)
PMechaOnPltSet_1=(+315.15,+116.04,+280.00,-29.59,+88.55,-30.45)(6,0)
PMechaOnPltSet_2=(+315.15,+116.04,+450.00,-29.59,+88.55,-30.45)(6,0)
PMechaOnPltSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet=(-132.57,+557.02,+307.25,-179.78,-0.31,-179.26)(7,0)
PMechaOnRoboGet_1=(-90.06,+557.02,+306.36,-179.78,-0.31,-179.25)(7,0)
PMechaOnRoboGet_2=(-70.00,+316.96,+419.50,+180.00,+0.00,+180.00)(7,0)
PMechaOnRoboGet_3=(+0.00,+350.00,+480.00,-180.00,+0.00,-180.00)(7,0)
PMechaOnRoboGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet=(-131.67,+557.03,+307.29,-179.78,-0.31,-179.26)(7,0)
PMechaOnRoboSet_1=(-90.06,+557.03,+307.29,-179.78,-0.31,-179.26)(7,0)
PMechaOnRoboSet_2=(+40.77,+316.96,+419.50,+180.00,+0.00,+180.00)(7,0)
PMechaOnRoboSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead=(+600.00,-152.00,+403.00,-180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+600.00,-152.00,+450.00,-180.00,+0.00,+90.00)(7,0)
PTicketRead_2=(+198.38,+259.83,+450.00,-180.00,+0.00,+128.47)(7,0)
PTicketRead_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
JActive=(-0.28,-16.41,+134.83,-0.95,-6.37,+0.85)
Jmove=(-0.28,-15.80,+124.16,+0.00,+71.59,+0.85)
JTaihi=(+0.00,-15.80,+124.16,+0.00,+71.59,+0.00)
