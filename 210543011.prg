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
48 Def Inte MKeyNum                    'KEY���͎󂯎��p(�ǉ�12/20����)
49 '===== <Insight�ϐ��ݒ�> =====
50 MIN_IS_Ready%        =   11380      '�y����IO�zInsight����OK
51 MIN_IS_JobLoadOK%    =   11381      '�y����IO�zInsight�W���u���[�h����I��
52 MIN_IS_JobLoadNG%    =   11382      '�y����IO�zInsight�W���u���[�h�ُ�I��
53 MIN_IS_InspGSetOK%   =   11383      '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
54 MIN_IS_InspGSetNG%   =   11384      '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
55 MIN_IS_InspOK%       =   11385      '�y����IO�zInsight����OK
56 MIN_IS_InspNG%       =   11386      '�y����IO�zInsight����NG
57 MIN_IS_InspErr%      =   11387      '�y����IO�zInsight�����ُ�I��
58 MIN_IS_InspCapDone%  =   11388      '�y����IO�zInsight�����摜�捞����
59 MIN_IS_ErrNum%       =   11408      '�y����IO�zInsight�����G���[�ԍ��J�n�A�h���X(16bit)
60 'Output Signal
61 MOUT_IS_JobLoadReq%  =   12370      '�y�o��IO�zInsight JOB���[�h�v��
62 MOUT_IS_InspGSetReq% =   12371      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
63 MOUT_IS_Insp%        =   12372      '�y�o��IO�zInsight �������s�v��
64 MOUT_IS_JobNum%      =   12384      '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
65 MOUT_IS_InspGNum%    =   12400      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
66 MOUT_InspErrNum%     =   12416      '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
67 MOUT_InspNGStepNum%  =   12432      '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
68 '===== <�d�h���ϐ���`> =====
69 X20_Driver=11248                    '�d�h���X�e�C�^�X1�@Driver Status 1
70 X21_Driver=11249 '�d�h���X�e�C�^�X2  Driver Status 2
71 X22_Driver=11250 '�d�h���X�e�C�^�X3  Driver Status 3
72 X23_Driver=11251 '�d�h���X�e�C�^�X4  Driver Status 4
73 X24_Driver=11252 '�d�h���G���[���b�Z�[�W1 Driver Error E1
74 X25_Driver=11253 '�d�h���G���[���b�Z�[�W2 Driver Error E2
75 X26_Driver=11254 '�d�h���G���[���b�Z�[�W3 Driver Error E3
76 X27_Driver=11255 '�d�h���G���[���b�Z�[�W4 Driver Error E4
77 X28_Driver=11256 '�d�h���g�[�^���G���[�V�O�i�� Total Error
78 X29_Driver=11257 '�d�h���I���V�O�i�� Comlete signal
79 X2A_Driver=11258 '�d�h���G���[���b�Z�[�W5 Driver Error E5
80 '11584   'toRB�g���N�h���C�o-COMP_ERR���M
81 Y60_Driver=12240 '�d�h�������v��� CCW
82 Y61_Driver=12241 '�d�h�����v��� CW
83 Y62_Driver=12242 '�o���N�Z�b�e�B���O BANK C1
84 Y63_Driver=12243 '�o���N�Z�b�e�B���O BANK C2
85 Y64_Driver=12244 '�o���N�Z�b�e�B���O BANK C3
86 Y65_Driver=12245 '�v���O�����Z�b�e�B���O PRG SET F1
87 Y66_Driver=12246 '�v���O�����Z�b�e�B���O PRG SET F2
88 Y67_Driver=12247 '�v���O�����Z�b�e�B���O PRG SET F3
89 X34_ScrewReady1=11259 '�˂�����1�@Read
90 '===== <�d�h���萔> =====
91 Dim PScrewPos(10)       '�l�W���ߗpFunction�����ϐ�
92 Dim PGetScrewPos(10)    '�˂������@����˂��𓾂�Function�����ϐ�
93 Dim PEscapePosi(10)
94 MLoopCnt% = 0'
95 '===== <���{�b�g�萔> =====
96 '===== <���{�b�g�ϐ���`> =====
97 MRBTOpeGroupNo = 0      '���{�b�g����ԍ�������
98 MCommentD1001 = 0
99 MCommentD1002 = 0
100 MCommentD1003 = 0
101 MScreenNo = 0
102 '
103 MCommentTSU = 0
104 MCommentTSD = 0
105 '�E�B���h��ʔԍ��ݒ�
106 MWindReSet = 0
107 MWindInfoScr = 5
108 MWindErrScr = 10
109 MWindErrScr2 = 11
110 MWindErrScr3 = 13
111 MWindErrScr17 = 17
112 MWindErrScr18 = 18
113 MWindCmmnScr = 20
114 MWindJigRelase19049 = 60
115 MWindJigRelase19050 = 61
116 MWindJigRelase19051 = 62
117 '
118 MClear% = 0        'KEY_�̃N���A
119 MAbout% = 1        'KEY_��~
120 MNext% = 2         'KEY_���̃X�e�b�v�ֈڍs
121 MContinue% = 3     'KEY_�p�� �ēx����������s��
122 '
123 MKeyNum% = 0       'KEY���͂��󂯎��
124 '
125 Def Inte MNgProcess
126 MNgProcess% = 5      'KEY_NG
127 '
128 MAssyOK% = 6       '�g������
129 MPass% = 7         '�H���p�X
130 MPiasNG% = 8       'Pias�m�F������NG
131 MIrregular% = 10   '��O�������s�p
132 MPiasOK% = 11      'PIAS��ǂ�OK��
133 '
134 '�������pKEY�ԍ�   '
135 MRobotInit1% = 11  '�����ʒu�p
136 MRobotInit2% = 12  '�����ʒu�p
137 MRobotInit3% = 13  '�����ʒu�p
138 MRobotInit4% = 14  '�����ʒu�p
139 '
140 MIN_INIT1REQUEST% = 11568 'toRBT_���{�b�g�����ʒu1�v��
141 MIN_INIT2REQUEST% = 11569 'toRBT_���{�b�g�����ʒu2�v��
142 MIN_INIT3REQUEST% = 11570 'toRBT_���{�b�g�����ʒu3�v��
143 MIN_INIT4REQUEST% = 11571 'toRBT_���{�b�g�����ʒu4�v��
144 '
145 MOUT_INIT1RECIVE% = 12560 'toPLC_���{�b�g�����ʒu1��M
146 MOUT_INIT2RECIVE% = 12561 'toPLC_���{�b�g�����ʒu2��M
147 MOUT_INIT3RECIVE% = 12562 'toPLC_���{�b�g�����ʒu3��M
148 MOUT_INIT4RECIVE% = 12563 'toPLC_���{�b�g�����ʒu4��M
149 '
150 MOK% = 1               '�e����p
151 MNG% = 0               '�e����p
152 MTIMEOUT% = -1         '�e����p
153 MJudge% = 0            '������i�[�p
154 '
155 MRECIVETIME& = 0
156 MSETTIMEOUT10& = 10000&                '10�b�ݒ�
157 MSETTIMEOUT08& = 8000&                 '10�b�ݒ�
158 MSETTIMEOUT03& = 3000&                 '3�b�ݒ�
159 MSETTIMEOUT01& = 1000&                 '1�b�ݒ�
160 MSETTIMEOUT05& = 5000&                 '5�b�ݒ�
161 MSETTIMEOUT009& = 900&                 '0.9�b�ݒ�
162 MSETTIMEOUT008& = 800&                 '0.8�b�ݒ�
163 MSETTIMEOUT007& = 700&                 '0.7�b�ݒ�
164 MSETTIMEOUT006& = 600&                 '0.6�b�ݒ�
165 MSETTIMEOUT005& = 500&                 '0.5�b�ݒ�
166 MSETTIMEOUT004& = 400&                 '0.4�b�ݒ�
167 MSETTIMEOUT003& = 300&                 '0.3�b�ݒ�
168 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
169 MIN_PIAS_ComOK% = 11552                'PC�ʐMOK
170 MIN_PIAS_ComTimeOut% = 11576           'PC�ʐM�m�F�^�C���A�E�g
171 MIN_PIAS_ComNG% = 11553                'PC�ʐMNG
172 MOUT_PIAS_ComCheck% = 12544            'PC�ʐM�m�F�v��
173 MOUT_PIAS_Missing_Process% = 12546     '�H�������m�F�v��
174 MIN_PIAS_ModelTypeNG% = 11554          '���f���d��NG
175 MIN_PIAS_ProcessHistryNG% = 11555      '�O�H������NG
176 MIN_PIAS_ProcessHistryOK% = 11556      '�O�H������OK
177 MIN_PIAS_ProcessHistryErr% = 11557     '�H�����������G���[
178 MIN_PIAS_MyProcessComp% = 11573        '���H����������
179 MIN_PIAS_ProcessHistryTimeOut% = 11578 '�H�������^�C���A�E�g
180 MOUT_OKNG% = 12226                     'PLC OUT ��OK=1, NG=0 �o��
181 '
182 MOUT_PiasPCBNumberCheck = 12557        '��ԍ��ƍ�
183 MIN_PiasPCBNumberOK% = 11566          '��ԍ�OK
184 MIN_PiasPCBNumberNG% = 11565          '��ԍ�NG
185 MIN_PiasPCBNumberErr% = 11567         '��ԍ������G���[
186 '
187 MOUT_PiasAssyResultOK% = 12549    '�g��OK
188 MOUT_PiasAssyResultNG% = 12550    '�g��NG
189 MOUT_PiasAssyResultWr% = 12548    '�H��������������
190 '
191 MIN_PiasProcessNG% = 11559        '�H����������NG
192 MIN_PiasProcessOtherErr% = 11560  '�H�����������G���[(�Ȃ񂩂̃g���u��)
193 MIN_PiasProcessOK% = 11558        '�H����������OK
194 '
195 MIN_Insight_Use% = 11374               '�摜�m�FON
196 MIN_TorqueCheck% = 11348               '�g���N�`�F�b�N
197 '
198 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT���쌠
199 MOUT_RED_LIGHT% = 12356            'PATLIGHT �� �_��
200 MOUT_RED_FLASH% = 12357            'PATLIGHT �� �_��
201 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT �� �_��
202 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT �� �_��
203 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT �� �_��
204 MOUT_GREEN_FLASH% = 12361          'PATLIGHT �� �_��
205 '
206 MOUT_ST_DATETIME% = 12551          '�g���J�n���t����
207 MOUT_ED_DATETIME% = 12552          '�g���I�����t����
208 '
209 MOUT_TORQUE_CHECK% = 12367         'PLC�փg���N�`�F�b�N���𑗐M
210 '
211 MIN_ASSY_CANCEL% = 11366           '�g�����s�����̃t���O
212 '
213 MLoopFlg% = 0                      'KEY���͌��OK or NG���e
214 MopeNo% = 0
215 MRtn% = 0
216 MRet = 0
217 MRet3% = 0
218 '
219 Def Inte MInputQty          '������ ���Z�ϐ�
220 Def Inte MAssyOkQty         '�g���n�j�� ���Z�ϐ�
221 Def Inte MAssyNgQty         '�g���m�f�� ���Z�ϐ�(���g�p)
222 Def Inte MSuctionErrQty     '�z���G���[�� ���Z�ϐ� 2022/04/27 �n��
223 Def Inte nAssyOkQty         '���g�p
224 Def Inte MScrewNo
225 Def Inte MReTry
226 '===== <IO�ϐ���`> =====
227 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
228 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
229 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
230 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
231 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
232 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
233 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
234 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
235 '
236 Def Inte Y6A_VV1            ' �A�[����[�@�l�W�z���o���u
237 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
238 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
239 '
240 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
241 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
242 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
243 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
244 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
245 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
246 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
247 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
248 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
249 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
250 '
251 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
252 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
253 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
254 '
255 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
256 '
257 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
258 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
259 '
260 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
261 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
262 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
263 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
264 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
265 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
266 '
267 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
268 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
269 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
270 '
271 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
272 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
273 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
274 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
275 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
276 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
277 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
278 Y6A_VV1%    =  12250    ' �A�[����[�@�l�W�z���o���u
279 Y6B_VB1%    =  12251    '�A�[����[�@�z���j��o���u
280 MOUT_VB1%   =  12251    ' �A�[����[�@�l�W�z���j��o���u
281 '
282 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
283 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
284 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
285 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
286 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
287 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
288 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
289 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
290 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
291 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
292 '
293 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
294 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
295 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
296 '
297 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
298 '
299 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
300 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
301 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
302 '
303 '����
304 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
305 Def Inte MOn                            '�o��=1
306 Def Inte MOff                           '�o��=0
307 '
308 '�˂����ߑ��u_�o�̓A�h���X
309 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
310 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
311 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
312 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
313 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
314 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
315 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
316 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
317 '�˂����ߑ��u_���̓A�h���X
318 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
319 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
320 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
321 Def Inte MIN_ScwT_Case1                 '����1��~����M
322 Def Inte MIN_ScwT_Case2                 '����2��~����M
323 Def Inte MIN_ScwT_Case3                 '����3��~����M
324 Def Inte MIN_ScwT_Case4                 '����4��~����M
325 Def Inte MIN_ScwT_Case5                 '����5��~����M
326 '
327 Dim MScwT_Case1%(2)               '����1��~�ϐ�
328 Dim MScwT_Case2%(2)               '����2��~�ϐ�
329 Dim MScwT_Case3%(2)               '����3��~�ϐ�
330 Dim MScwT_Case4%(2)               '����4��~�ϐ�
331 Dim MScwT_Case5%(2)               '����5��~�ϐ�
332 '
333 '����
334 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
335 MOn% = 1                                 '�o�� = 1
336 MOff% = 0                                '�o�� = 0
337 '
338 '�˂����ߋ@_�A�h���X�ݒ�
339 MOUT_ScwT_ComChk% = 12832               '�ʐM�m�F���M
340 MOUT_ScwT_ST% = 12865                   '�˂����ߊJ�n�𑗐M
341 MOUT_ScwT_ReSTOK% = 12866               '�ĊJ�n��M�𑗐M
342 MOUT_ScwT_FinOK% = 12868                '�˂����ߊ�����M�𑗐M
343 MOUT_ScwT_Case1OK% = 12858              '����1��~��M�𑗐M
344 MOUT_ScwT_Case2OK% = 12859              '����2��~��M�𑗐M
345 MOUT_ScwT_Case3OK% = 12860              '����3��~��M�𑗐M
346 MOUT_ScwT_Case4OK% = 12861              '����4��~��M�𑗐M
347 MOUT_ScwT_Case5OK% = 12862              '����5��~��M�𑗐M
348 '
349 MIN_ScwT_comOK% = 11840                 '�˂����ߑ��u����ԐM
350 MIN_ScwT_STRec% = 11873                 '�˂����ߊJ�n����M
351 MIN_ScwT_ReST% = 11874                  '�ĊJ�n����M
352 MIN_ScwT_Fin% = 11876                   '�˂����ߊ�������M
353 MIN_ScwT_Case1% = 11866                 '����1��~�ҋ@����M
354 MIN_ScwT_Case2% = 11867                 '����2��~�ҋ@����M
355 MIN_ScwT_Case3% = 11868                 '����3��~�ҋ@����M
356 MIN_ScwT_Case4% = 11869                 '����4��~�ҋ@����M
357 MIN_ScwT_Case5% = 11870                 '����5��~�ҋ@����M
358 '
359 MScwT_Case1%(1) = MIN_ScwT_Case1%
360 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
361 MScwT_Case2%(1) = MIN_ScwT_Case2%
362 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
363 MScwT_Case3%(1) = MIN_ScwT_Case3%
364 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
365 MScwT_Case4%(1) = MIN_ScwT_Case4%
366 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
367 MScwT_Case5%(1) = MIN_ScwT_Case5%
368 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
369 '
370 '�ݒ� InitialZoneB�Ŏg�p����ϐ�
371 Def Pos PActive       '�������W�n �ʒu�ϐ� ���݈ʒu
372 Def Pos Pmove         '�������W�n �ʒu�ϐ� �ړ���
373 Def Jnt JActive       '�֐ߍ��W�n �ʒu�ϐ� ���݈ʒu
374 Def Jnt Jmove         '�֐ߍ��W�n �ʒu�ϐ� �ړ���
375 Def Jnt JTaihi        '�֐ߍ��W�n �ʒu�ϐ� �ޔ��|�W�V���� �e�B�[�`���O�Őݒ�
376 Def Inte MRecoveryPass         '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s
377 Def Inte MStandby              '�ҋ@�ʒu�m�F�t���O
378 Def Inte MRecoveryChuckOpen    '�`���b�N����t���O�i���A����O�j���͂ݑ΍�
379 '�����Ӂ������ʒu��ύX�������ɂ́A�ύX���K�v�I
380 '
381 '
382 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
383 Function M% fnAssyStart
384     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
385 ' PIAS�`�P�b�g�Ǎ��ݍH�������m�F
386     M_20# = MClear%                       '������
387     If M_22# = MIrregular% Then GoTo *IRREGULAR     'Assy������DVD���J��c�����Ă���ꍇ
388     '�J�n�ʒu���C�j�V�����|�W�V�����������ꍇ(�n���h��������������`�F�b�N)
389     *RE_START
390     PTemp = P_Curr
391     MRtn = 0
392     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
393         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
394             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
395                 MRtn = 1
396             EndIf
397         EndIf
398     EndIf
399     '�C�j�V�����|�W�V�����ɂ����ꍇ�Z���T�[�`�F�b�N
400     If MRtn = 1 Then
401         Ovrd 20
402         Accel 100 , 20
403         Mvs PHandChange                             '�n���h�����ʒu�Ɉړ�
404         Ovrd 100
405         Accel 100 , 100
406         MRtn = frInCheck(11264,0,MSETTIMEOUT03&)    '�Z���T�[�`�F�b�N(���������)
407         Mvs PInitialPosition                        '�C�j�V�����|�W�V�����Ɉړ�
408     Else
409         MRtn = 1
410     EndIf
411     If MRtn = 1 Then GoTo *AssyStart
412     fErrorProcess(11,286,287,0)                        '�n���h�����p�������
413     If M_20# = MNext% Then M_20# = MClear%
414     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
415     If M_20# = MNgProcess% Then GoTo *RE_START
416     If M_20# = MContinue% Then GoTo *RE_START
417 '
418     *AssyStart
419 '
420 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
421 '        MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
422 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
423 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
424 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' ���j���[�֖߂�
425 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NG���H�������ɏ����ݎ��̍H����
426 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NG���H�������ɏ����ݎ��̍H����
427 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' ����NG, �H������
428 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK�ȊO�͑g���I��
429 '    EndIf
430 ' �l�W���ߋ@�e�X�g�p ----------
431 '    'Mret% = fScewTcomChk()
432 '    '�˂����ߊJ�n
433     MRtn2 = fScewTStart()
434 '    '
435 '    '���W�ړ�
436 '    '
437 '    '����xx��~
438 '    fScewTCaseStop(MScwT_Case5%)
439 '    '
440 '    '�x�[�X���j�b�gKEY
441 '    Wait M_In(MTEST_KEY%) = MOn%
442 '    '
443 '    '�ĊJ�n
444 '    fScewTReStart()
445 '    '
446 '    '���W�ړ�
447 '    '
448 '    '�˂����ߊ���
449 '    Mret% = fScewTFinish()
450 ' �l�W���߃e�X�g�I��
451 ' PIAS�e�X�g -----------
452 '    MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
453 '    MRet% = fnPiasWrite(MNG%)
454  '   MRet% = fnPCBNumberCheck()     '�f�o�b�N�p�ɃR�����g�A�E�g(9/17����)
455 ' PIAS�e�X�g�I�� -------
456 '�g�ݗ��ĊJ�n(���g��9/10����)
457 '�v���O�������_
458 Ovrd 100
459 If MRtn2 = 0 Then GoTo *INITIAL_CHECK
460     fErrorProcess(11,329,201,0)
461     If M_20# = MNext% Then GoTo *AssyStart
462     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
463     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
464     If M_20# = MContinue% Then GoTo *AssyStart
465 '�n���h��DVD���J,�u���P�b�g��������
466 '
467 *INITIAL_CHECK
468 '
469 If M_In(11264) = 0 And M_In(11267) = 0 And M_In(11270) = 0 Then GoTo *CompInitial1  'DVD���J,�u���P�b�g��������
470 fErrorProcess(11,253,287,0)                 '284��287�ɕύX6/2����
471 If M_20# = MNext% Then M_20# = MClear%
472 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
473 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
474 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
475 *CompInitial1
476 '
477 '�n���h���C�j�V�����ɖ߂�
478 If M_In(11266) = 1 Then     'DVD�`���b�N���o
479     M_Out(12256) = 0        'DVD�`���b�N��OFF
480     M_Out(12257) = 1        'DVD�`���b�N�JON
481     Break
482 EndIf
483 If M_In(11269) = 1 Then     'F�V�����_�[�o���o
484     M_Out(12258) = 0        'F�V�����_�[�oOFF
485     M_Out(12259) = 1        'F�V�����_�[��ON
486     Break
487 EndIf
488 If M_In(11272) = 1 Then     'R�V�����_�[�o���o
489     M_Out(12260) = 0        'R�V�����_�[�oOFF
490     M_Out(12261) = 1        'R�V�����_�[��ON
491     Break
492 EndIf
493 '
494 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)    'DVD�`���b�N�J���o
495 If MRtn = 1 Then GoTo *CompInitial2
496 fErrorProcess(11,270,284,0)
497 If M_20# = MNext% Then M_20# = MClear%
498 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
499 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
500 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
501 *CompInitial2
502 '
503 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)    'F�V�����_�[�ߌ��o
504 If MRtn = 1 Then GoTo *CompInitial3
505 fErrorProcess(11,278,284,0)
506 If M_20# = MNext% Then M_20# = MClear%
507 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
508 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
509 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
510 *CompInitial3
511 '
512 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)    'R�V�����_�[�ߌ��o
513 If MRtn = 1 Then GoTo *CompInitial4
514 fErrorProcess(11,276,284,0)
515 If M_20# = MNext% Then M_20# = MClear%
516 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
517 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
518 If M_20# = MContinue% Then GoTo *INITIAL_CHECK
519 *CompInitial4
520 '
521 '
522 ' 2022/04/11 ���S�����֏����ǉ� �n��
523 ' PInitialPosition �ݐ� MStandby=1
524 ' PMechaOnJigGet_3 �ݐ� MStandby=2
525 '
526 MStandby = 0    '�ҋ@�ʒu�t���O��������
527 PTemp = P_Curr
528 If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
529     If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
530         If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
531             MRtn = 1
532         EndIf
533     EndIf
534 EndIf
535 If (PTemp.X <= PMechaOnJigGet_3.X + 1.0) And (PTemp.X >= PMechaOnJigGet_3.X - 1.0) Then
536     If ((PTemp.Y <= PMechaOnJigGet_3.Y + 1.0) And (PTemp.Y >= PMechaOnJigGet_3.Y - 1.0)) Then
537         If ((PTemp.Z <= PMechaOnJigGet_3.Z + 1.0) And (PTemp.Z >= PMechaOnJigGet_3.Z - 1.0)) Then
538             MRtn = 2
539         EndIf
540     EndIf
541 EndIf
542 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
543     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
544         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
545             MStandby = 3
546         EndIf
547     EndIf
548 EndIf
549 If MRtn = 0 Then                   '�����ʒu�ɂ��Ȃ����̓G���[�ɂ���
550     fErrorProcess(11,230,281,0)    '�G���[��~
551     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
552     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
553     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
554     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
555 EndIf
556 '
557 '
558 'DVD����PASS�v���O��������ʏ�v���O�����ɐؑւ������̑΍� 2022.05.13 �n��
559 PTemp = P_Curr
560 If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
561     If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
562         If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
563             Ovrd 50
564             Mov PInitialPosition
565             Ovrd 100
566         EndIf
567     EndIf
568 EndIf
569 '
570 '
571 *ReCheck1
572     M_Out(12912) = 1                  'Ver 0.4 �ǉ��@���u����t���O����(�H��6�D��̂��ߍH��5�̃t���O�Ď��̑O�ɏo��)
573     Mov PMechaOnJigGet_3    '�^�N�g�Z�k�̂��ߏ����ʒu�ύX
574     'Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�����ʒu�ړ�2/9����)
575     MRet = fTimeOutJudge(11920,0)      'BaseUnit5���u����t���O�m�F(�^�C���A�E�g������ǉ�22/09/29����)
576     If MRet = 0 Then GoTo *ASSY_ERROR_END
577     If MRet = 2 Then GoTo *ReCheck1
578     M_Out(12912) = 1                  '���u����t���O����(�O�̂��ߒǉ�2/9����)
579 '
580 'Ver 0.4 �ǉ�--------------------
581 If M_In(11920) = 1 Then GoTo *CompInitial4         '�H��6�����쒆�̏ꍇ11920 = 1 ���[�v
582 'Ver 0.4 �����܂�----------------           '�H��6�D��̂���12912=0�ɂ��Ȃ�
583 '
584 'Mov PInitialPosition   '1/20�R�����g�A�E�g(����)
585 '
586 '���u������_�ֈړ�
587 Mov PMechaOnJigGet_2         '���u������_
588 '
589 '���u���䂩��DVD���J���󂯎��
590 'Wait M_In(12914) = 1              '����������M
591 'Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�ǉ���������10/1����)
592 '
593     MRtn2 = 0                   '�G���[�����ւ������ꂽ���p
594 *RE_JIG_GET
595     M_20# = MClear%
596 '
597     M_Out(12912) = 1                  '���u����t���O����
598     If M_In(11278) = 1 Then          '�����̊m�F(CW�[�Z���T�[)(�ǉ������܂�10/1����)
599         Mov PMechaOnJigGet1_1    '���u������
600         Ovrd 25
601         Mvs PMechaOnJigGet1      'DVD���J�󂯎��ʒu
602         M_Out(12257) = 0         'DVD���J�`���b�N�JOFF
603         M_Out(12256) = 1         'DVD���J�`���b�N��ON
604 '        Wait M_In(11266) = 1     'DVD���J�`���b�N���o
605         MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
606         If MRtn = 0 And MRtn2 = 0 Then             'DVD���J��c���ł��Ȃ������ꍇ�`���b�N���J��(6/2����)
607             M_Out(12256) = 0         'DVD���J�`���b�N��OFF
608             M_Out(12257) = 1         'DVD���J�`���b�N�JON
609         EndIf
610         Mvs PMechaOnJigGet1_1    '���u������
611         Ovrd 100
612         Break
613     ElseIf M_In(11277) = 1 Then       '�����̊m�F(CCW�[�Z���T�[)(�ǉ���������10/1����)
614         Mov PMechaOnJigGet2_1    '���u������
615         Ovrd 25
616         Mvs PMechaOnJigGet2      'DVD���J�󂯎��ʒu
617         M_Out(12257) = 0         'DVD���J�`���b�N�JOFF
618         M_Out(12256) = 1         'DVD���J�`���b�N��ON
619 '        Wait M_In(11266) = 1     'DVD���J�`���b�N���o
620         MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
621         If MRtn = 0 And MRtn2 = 0 Then             'DVD���J��c���ł��Ȃ������ꍇ�`���b�N���J��(6/2����)
622             M_Out(12256) = 0         'DVD���J�`���b�N��OFF
623             M_Out(12257) = 1         'DVD���J�`���b�N�JON
624         EndIf
625         Mvs PMechaOnJigGet2_1    '���u������
626         Ovrd 100
627         Break
628     EndIf
629 '
630     If MRtn = 1 Or MRtn2 = 1 Then GoTo *CompMechaGet1    'DVD���J�`���b�N�G���[����(���ւ������ꂽ�����G���[�\���Ȃ��Ő�ɐi�ނ悤�ɕύX(6/2����))
631 '    PTemp = P_Curr              '�����ʒu�̕ύX1/12����
632 '    Mvs PTemp , -150
633     Mov PMechaOnJigGet_2
634     fErrorProcess(11,269,294,0)'11,279,284,0����11,269,294,0�ɕύX(6/2����)
635 '    PTemp = P_Curr             '�����ʒu�̕ύX1/12����
636 '    Mvs PTemp , -150
637 '    Mov PMechaOnJigGet_2
638     If M_20# = MNext% Then
639         M_20# = MClear%
640         MRtn2 = 1
641     EndIf
642     If M_20# = MContinue% Then MRtn2 = 0
643     If M_20# = MAbout% Or M_20# = MNgProcess% Then
644         Mov PInitialPosition
645         Break
646     EndIf
647     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
648     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
649     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_JIG_GET
650 *CompMechaGet1
651 '
652     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
653 Mov PMechaOnJigGet_2            '���u������_
654     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
655 '
656 'DVD���J�Z���T�[���o
657 '
658 'M_Out(12912) = 0                  '���u����t���O���(�����ʒu�ύX2/9����)
659 '
660 '
661 '    ' ���i�����v�����M(�����ʒu�ύX3/31����)
662     M_Out(12787) = 1
663 'DVD���J���˂����{3�ɒu��
664 Mov PMechaOnRoboSet_2        '�˂����{���_
665 '
666 'Wait M_In(11888) = 1         '�˂����{3��~1�܂őҋ@
667 MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
668 If MRtn = 0 Then Mov PInitialPosition     '"�C�j�V�����ɖ߂铮��"
669 If MRtn = 0 Then GoTo *ASSY_ERROR_END
670 '
671 Mov PMechaOnRoboSet_1        '�˂����{���
672 Ovrd 25
673 Mvs PMechaOnRoboSet          'DVD���J�u����
674 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~1�`��~2�܂�)
675 '
676 'Wait M_In(11889) = 1         '�˂����{3��~2�܂őҋ@
677 MRtn = fScrewTighenRoboCheck(11889)    '��~��Ԃ���M����
678 'If MRtn = 0 Then
679 '    M_Out(12256) = 0             'DVD���J�`���b�N��OFF
680 '    M_Out(12257) = 1             'DVD���J�`���b�N�JON
681 '    Mvs PMechaOnRoboSet_1
682 '    Mov PMechaOnRoboSet_2
683 '    Mov PInitialPosition
684 'EndIf
685 If MRtn = 0 Then GoTo *ASSY_ERROR_END   '���̏�Œ�~����(�̏�̉\��)
686 '
687 *RE_ROBO_SET_1
688 '
689 M_Out(12256) = 0             'DVD���J�`���b�N��OFF
690 M_Out(12257) = 1             'DVD���J�`���b�N�JON
691 '
692 'Wait M_In(11265) = 1         'DVD���J�`���b�N�J���o
693 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
694 If MRtn = 1 Then GoTo *CompRoboSet1
695 fErrorProcess(11,270,284,0)
696 If M_20# = MNext% Then M_20# = MClear%
697 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
698 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
699 If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
700 *CompRoboSet1
701 '
702 ''    ' ���i�����v�����M(�����ʒu�ύX3/31����)
703 '    M_Out(12787) = 1
704 '
705 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~2�`��~3�܂�)
706 '
707 'Wait M_In(11890) = 1         '�˂����{3��~3�܂őҋ@
708 MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����
709 MScrewRoboNgFlg% = 0
710 If MRtn = 0 Then MScrewRoboNgFlg% = 1
711 '
712 Mvs PMechaOnRoboSet_1        '�˂����{���
713 Ovrd 100
714 Mov PMechaOnRoboSet_2        '�˂����{���_
715 M_Out(12912) = 0                  '���u����t���O���(�����ʒu�ύX2/9����)
716 '
717 '
718 If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition   '�˂����{�Œ�~��NG��������Ă����ꍇ
719 If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END   '
720 '
721 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~3�`��~4�܂�)
722 '
723 ''    ' ���i�����v�����M(�����ʒu�ʒu�ύX1/20����)
724 '    M_Out(12787) = 1
725 '    '    ' ���i���������҂�
726 '    Wait M_In(11810) = 1
727 '
728 'DVD����(R)�����
729 *RE_R_GET_3
730 Mov PBracketRGet_3           '�o�H
731 Mov PBracketRGet_2           '����(R)�󂯎����_
732 M_Out(12261) = 0             '����(R)�V�����_�[��OFF
733 M_Out(12260) = 1             '����(R)�V�����_�[�oON
734 '
735     '    ' ���i���������҂�(�����ύX2/27����)
736 *RE_FEEDER_READY
737 '    Wait M_In(11810) = 1
738     fnAutoScreenComment(513)    '��ԕ\��[���i�����҂�] 2022/04/26 �n��
739 'MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '�����҂�
740 MRtn = frInCheck(11810,1,MSETTIMEOUT08&)   '�����҂�
741 If MRtn = 1 Then GoTo *CompFeederReady
742 '    ' ���i�����v���I��
743     M_Out(12787) = 0
744 fErrorProcess(11,289,284,0)
745 If M_20# = MNext% Then M_20# = MClear%
746 If M_20# = MAbout% Or M_20# = MNgProcess% Then
747     Mov PBracketRGet_2
748     Mov PBracketRGet_3
749     Mov PBracketRSet_3
750     Mov PInitialPosition1
751 EndIf
752 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
753 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
754 '    ' ���i�����v��
755     M_Out(12787) = 1
756 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
757 *CompFeederReady
758 '    ' ���i�����v���I��
759     M_Out(12787) = 0
760 '
761     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
762 Mov PBracketRGet_1           '����(R)�󂯎����
763 '
764 *RE_R_GET_1
765 '
766 If M_20# = MContinue% Then
767     M_Out(12261) = 0             '����(R)�V�����_�[��OFF
768     M_Out(12260) = 1             '����(R)�V�����_�[�oON
769     M_20# = MClear%
770 EndIf
771 '
772 'Wait M_In(11272) = 1         '����(R)�V�����_�[�o�[���o
773     fnAutoScreenComment(513)    '��ԕ\��[���i�����҂�] 2022/04/26 �n��
774 MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '����(R)�V�����_�[�o�[���o
775 If MRtn = 1 Then GoTo *CompRGet1
776 fErrorProcess(11,275,284,0)
777 If M_20# = MNext% Then M_20# = MClear%
778 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
779 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
780 If M_20# = MContinue% Then GoTo *RE_R_GET_1
781 *CompRGet1
782 '
783 Ovrd 25
784 '
785 *RE_R_GET_2
786 '
787     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
788 Mvs PBracketRGet             '����(R)�󂯎��ʒu
789 '
790 '
791 M_Out(12252) = 0             '�^��OFF�o���uOFF
792 M_Out(12253) = 0             '�^��j��o���uOFF(�O�̂���)
793 M_Out(12251) = 1             '�^��ON�o���uON
794 '
795 'Wait M_In(11270) = 1         '����(R)�z���Z���T�[ON���o
796 MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   '����(R)�z���Z���T�[ON���o
797 If MRtn = 1 Then GoTo *CompRGet2
798 Mvs PBracketRGet_1           '����(R)�󂯎����
799 fErrorProcess(11,279,295,0)  '284��295�֕ύX6/2����
800 If M_20# = MNext% Then M_20# = MClear%
801 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
802 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
803 If M_20# = MContinue% Then GoTo *RE_R_GET_2
804 *CompRGet2
805 '
806 Mvs PBracketRGet_1           '����(R)�󂯎����
807 Ovrd 100
808 Mov PBracketRGet_2           '����(R)�󂯎����_
809 Mov PBracketRGet_3           '�o�H
810 '
811 'DVD����(R)��u��
812 Mov PBracketRSet_3           '���_
813 MRtn = frInCheck(11270,1,MSETTIMEOUT05&)              '������x����(R)�z���Z���T�[ON���o
814 If MRtn = 1 Then GoTo *CompRGet3
815 fErrorProcess(11,279,295,0)  '284��295�ɕύX6/2����
816 If M_20# = MNext% Then M_20# = MClear%
817 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
818 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
819 If M_20# = MContinue% Then GoTo *RE_R_GET_3
820 *CompRGet3
821 '
822 'Wait M_In(11891) = 1         '�˂����{3��~4�܂őҋ@
823 MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
824 If MRtn = 0 Then Mov PInitialPosition
825 If MRtn = 0 Then GoTo *ASSY_ERROR_END
826 '
827 Mov PBracketRSet_2           '����(R)�u���ʒu���_
828 Mvs PBracketRSet_1           '����(R)�u���ʒu���
829 *RE_R_SET_1                  '�߂��ʒu(6/2�ύX,����)
830 Ovrd 10
831 Mvs PBracketRSet             '����(R)�u���ʒu
832 Dly 0.1
833 '
834 '*RE_R_SET_1                  '�߂��ʒu(6/2�ύX,����)
835 '
836 M_Out(12251) = 0             '�^��ON�o���uOFF
837 M_Out(12252) = 1             '�^��OFF�o���uON
838 M_Out(12253) = 1             '�^��j��o���uON
839 '
840 MRtn = frInCheck(11270,0,MSETTIMEOUT05&)
841 '
842 Dly 0.2
843 'M_Out(12253) = 0             '�^��j��o���uOFF
844 '
845 If MRtn = 1 Then GoTo *CompRSet1
846 Mvs PBracketRSet_1           '����(R)�u���ʒu���
847 M_Out(12253) = 0             '�^��j��o���uOFF
848 fErrorProcess(11,296,297,0)  '236,284��296,297�ɕύX6/2����
849 If M_20# = MNext% Then M_20# = MClear%
850 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
851 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
852 If M_20# = MContinue% Then GoTo *RE_R_SET_1
853 *CompRSet1
854 '
855 'M_Out(12260) = 0             '����(R)�V�����_�[�oOFF(�߂��ʒu�ύX1/20����)
856 'M_Out(12261) = 1             '����(R)�V�����_�[��ON
857 ''
858 '*RE_R_SET_2
859 ''
860 ''Wait M_In(11271) = 1         '����(R)�V�����_�[�ߒ[���o
861 'MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   '����(R)�V�����_�[�ߒ[���o
862 'If MRtn = 1 Then GoTo *CompRSet2
863 'fErrorProcess(11,276,284,0)
864 'If M_20# = MNext% Then M_20# = MClear%
865 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
866 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
867 'If M_20# = MContinue% Then GoTo *RE_R_SET_2
868 '*CompRSet2
869 '
870 Ovrd 1                       '�u���P�b�g�u���ʒu����΍�(5/17����)
871 Mvs PBracketRSet , -5        '�u���P�b�g�u���ʒu����΍�(5/17����)
872 Ovrd 100
873 Mvs PBracketRSet_1           '����(R)�u���ʒu���
874 M_Out(12253) = 0             '�^��j��o���uOFF
875 Ovrd 100
876 Mov PBracketRSet_2           '����(R)�u���ʒu���_
877 Mov PBracketRSet_3           '���_
878 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~4�`��~5�܂�)
879 '
880 *RE_R_SET_2
881 '
882 M_Out(12260) = 0             '����(R)�V�����_�[�oOFF(�߂��ʒu�ύX1/20����)
883 M_Out(12261) = 1             '����(R)�V�����_�[��ON
884 '
885 'Wait M_In(11271) = 1         '����(R)�V�����_�[�ߒ[���o
886 MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   '����(R)�V�����_�[�ߒ[���o
887 If MRtn = 1 Then GoTo *CompRSet2
888 fErrorProcess(11,276,284,0)
889 If M_20# = MNext% Then M_20# = MClear%
890 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
891 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
892 If M_20# = MContinue% Then GoTo *RE_R_SET_2
893 *CompRSet2
894 '
895 '
896 '�u���ʒu�摜����(�����ʒu�ړ�)
897 'Wait M_In(11892) = 1         '�˂����{3��~5�܂őҋ@(�摜��������F���u���P�b�g�u��)
898 'MRtn = fScrewTighenRoboCheck(11892)    '��~��Ԃ���M����
899 'If MRtn = 0 Then Mov PInitialPosition
900 'If MRtn = 0 Then GoTo *ASSY_ERROR_END
901 '
902 'Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�ǉ���������10/1����)
903 ''
904 'M_Out(12912) = 1                  '���u����t���O����(�Փ˖h�~)
905 ''
906 ''Mov PBracketRCheck_2         '�o�H
907 ''Mov PBracketRCheck           '�����ʒu
908 ''
909 '*RE_R_CHECK
910 ''If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
911 'PInspPosition(1) = PBracketRCheck1
912 'MInspGroup%(1) = 2
913 'PInspPosition(2) = PBracketRCheck2
914 'MInspGroup%(2) = 3
915 'MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
916 'If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
917 'If MRtn = 0 Then
918 '    MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
919 'EndIf
920 'If MRtn = 1 Then GoTo *CompRCheck
921 'fErrorProcess(11,43,46,0)
922 'If M_20# = MNext% Then M_20# = MClear%
923 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
924 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
925 'If M_20# = MContinue% Then GoTo *RE_R_CHECK
926 '*CompRCheck
927 '*SkipCheck1
928 '
929 'M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~5�`��~6�܂�)
930 '
931 'Mov PBracketRCheck
932 '
933 'DVD����(F)�����
934 *RE_F_GET_3
935 'M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~5�`��~6�܂�)(�����ʒu�ύX1/20����)
936 Mov PBracketFGet_3           '���_
937 '
938 M_Out(12912) = 0                  '���u����t���O���(�Փ˖h�~)
939 '
940 Mov PBracketFGet_2           '����(F)�󂯎����_
941 'Mov PBracketFGet_1           '����(F)�󂯎����(�ړ��^�C�~���O�ύX1/20����)
942 '
943 *RE_F_GET_1
944 '
945 M_Out(12259) = 0             '����(F)�V�����_�[��OFF
946 M_Out(12258) = 1             '����(F)�V�����_�[�oON
947 '
948 Mov PBracketFGet_1           '����(F)�󂯎����(�ړ��^�C�~���O�ύX1/20����)
949 '
950 'Wait M_In(11269) = 1         '����(F)�V�����_�[�o�[���o
951 MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   '����(F)�V�����_�[�o�[���o
952 If MRtn = 1 Then GoTo *CompFGet1
953 fErrorProcess(11,277,284,0)
954 If M_20# = MNext% Then M_20# = MClear%
955 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
956 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
957 If M_20# = MContinue% Then GoTo *RE_F_GET_1
958 *CompFGet1
959 '
960 Ovrd 25
961 *RE_F_GET_2
962 Mvs PBracketFGet             '����(F)�󂯎��ʒu
963 '
964 '
965 '
966 M_Out(12249) = 0             '�^��OFF�o���uOFF
967 M_Out(12250) = 0             '�^��j��o���uOFF
968 M_Out(12248) = 1             '�^��ON�o���uON
969 '
970 'Wait M_In(11267) = 1         '����(F)�z���Z���T�[ON���o
971 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)   '����(F)�z���Z���T�[ON���o
972 If MRtn = 1 Then GoTo *CompFGet2
973 Mvs PBracketFGet_1           '����(F)�󂯎����
974 fErrorProcess(11,280,295,0)  '284��295�ɕύX6/2����
975 If M_20# = MNext% Then M_20# = MClear%
976 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
977 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
978 If M_20# = MContinue% Then GoTo *RE_F_GET_2
979 *CompFGet2
980 '
981 Mvs PBracketFGet_1           '����(F)�󂯎����
982 'Ovrd 100
983 Mov PBracketFGet_2           '����(F)�󂯎����_
984 Mov PBracketFGet_3           '���_
985 '
986 MRtn = frInCheck(11267,1,MSETTIMEOUT05&)          '������x����(F)�z���Z���T�[ON���o
987 If MRtn = 1 Then GoTo *CompFGet3
988 fErrorProcess(11,280,295,0)  '284��295�ɕύX6/2����
989 If M_20# = MNext% Then M_20# = MClear%
990 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
991 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
992 If M_20# = MContinue% Then GoTo *RE_F_GET_3
993 *CompFGet3
994 '
995 'DVD����(F)��u��
996 Mov PBracketFSet_3           '���_
997 '    ' ���i�����v���I��
998     M_Out(12787) = 0
999 '    ' ���i�擾�������M(�p���X)
1000     M_Out(12800) = 1 Dly 0.5
1001     '
1002 Ovrd 70
1003 Mov PBracketFSet_2           '����(F)�u���ʒu���_
1004 'Wait M_In(11893) = 1         '�˂����{3��~5�܂őҋ@
1005 MRtn = fScrewTighenRoboCheck(11892)    '��~��Ԃ���M����
1006 If MRtn = 0 Then Mov PInitialPosition1  '��~�ʒu�Ɉړ�
1007 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1008 '
1009     fnAutoScreenComment(533)    '��ԕ\��[�H���T�̃��{����I���҂�] 2022/04/26 �n��
1010 Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�ǉ���������10/1����)
1011 '
1012 M_Out(12912) = 1                  '���u����t���O����(�Փ˖h�~)
1013 '
1014     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1015 'Ovrd 70
1016 'Mov PBracketFSet_2           '����(F)�u���ʒu���_
1017 Mvs PBracketFSet_1           '����(F)�u���ʒu���
1018 *RE_F_SET_1                  '�߂��ʒu(�ύX6/2����)
1019 Ovrd 10
1020 Mvs PBracketFSet             '����(F)�u���ʒu
1021 Dly 0.2
1022 '
1023 '*RE_F_SET_1                  '�߂��ʒu(�ύX6/2����)
1024 '
1025 M_Out(12248) = 0             '�^��ON�o���uOFF
1026 M_Out(12249) = 1             '�^��OFF�o���uON
1027 M_Out(12250) = 1             '�^��j��o���uON
1028 '
1029 MRtn = frInCheck(11267,0,MSETTIMEOUT05&)
1030 Dly 0.2
1031 'M_Out(12250) = 0             '�^��j��o���uOFF
1032 '
1033 If MRtn = 1 Then GoTo *CompFSet1
1034 M_Out(12250) = 0             '�^��j��o���uOFF
1035 fErrorProcess(11,296,297,0)  '236,284��296,297�ɕύX6/2����
1036 If M_20# = MNext% Then M_20# = MClear%
1037 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1038 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1039 If M_20# = MContinue% Then GoTo *RE_F_SET_1
1040 *CompFSet1
1041 '
1042 '
1043 '
1044 '*RE_F_SET_2'(�߂��ʒu�ύX)
1045 ''
1046 'M_Out(12258) = 0             '����(F)�V�����_�[�oOFF
1047 'M_Out(12259) = 1             '����(F)�V�����_�[��ON
1048 ''
1049 ''Wait M_In(11268) = 1         '����(F)�V�����_�[�ߒ[���o
1050 'MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '����(F)�V�����_�[�ߒ[���o
1051 'If MRtn = 1 Then GoTo *CompFSet2
1052 'fErrorProcess(11,277,284,0)
1053 'If M_20# = MNext% Then M_20# = MClear%
1054 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1055 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1056 'If M_20# = MContinue% Then GoTo *RE_F_SET_2
1057 '*CompFSet2
1058 '
1059 Ovrd 1                       '�u���P�b�g�u���ʒu����΍�(5/17����)
1060 Mvs PBracketFSet , -5        '�u���P�b�g�u���ʒu����΍�(5/17����)
1061 Ovrd 100
1062 Mvs PBracketFSet_1           '����(F)�u���ʒu���
1063 M_Out(12250) = 0             '�^��j��o���uOFF
1064 Ovrd 100
1065 Mvs PBracketFSet_2           '����(F)�u���ʒu���_
1066 M_Out(12258) = 0             '����(F)�V�����_�[�oOFF(�ǉ�3/31����)
1067 M_Out(12259) = 1             '����(F)�V�����_�[��ON(�ǉ�3/31����)
1068 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~5�`��~6�܂�)
1069 Mov PBracketFSet_3           '���_
1070 M_Out(12912) = 0             '���u����t���O���
1071 'M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~5�`��~6�܂�)
1072 *RE_F_SET_2'(�߂��ʒu�ύX)
1073 '
1074 M_Out(12258) = 0             '����(F)�V�����_�[�oOFF
1075 M_Out(12259) = 1             '����(F)�V�����_�[��ON
1076 '
1077 'Wait M_In(11268) = 1         '����(F)�V�����_�[�ߒ[���o
1078 MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   '����(F)�V�����_�[�ߒ[���o
1079 If MRtn = 1 Then GoTo *CompFSet2
1080 fErrorProcess(11,277,284,0)
1081 If M_20# = MNext% Then M_20# = MClear%
1082 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1083 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1084 If M_20# = MContinue% Then GoTo *RE_F_SET_2
1085 *CompFSet2
1086 '
1087 '----------PIAS��ǂ�----------
1088 '�T�v�F�ǂ݂ɍs�����ǂ��������߂�Ƃ��Ƀ^�C���A�E�g�Ő�ɐi�߂�悤�ɂ���^�C�}�[
1089     Mov PTicketRead_2
1090     Mov PTicketRead_1
1091     M_22# = MClear%
1092     M_20# = MClear%
1093     M_Timer(4) = 0
1094     MloopFlg = 0
1095     While MloopFlg = 0
1096         MCrtTime& = M_Timer(4)
1097         If M_In(11354) = 1 Then
1098             MloopFlg = 1
1099         ElseIf MCrtTime& > 5000 Then    '�b��5�b(�������Ă���͕̂b���������₷�����Ă����)
1100             MloopFlg = 1
1101         EndIf
1102     WEnd
1103     MRtn = 0
1104     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
1105         If M_In(11354) = 1 Then         '�g���J�n��ON�Ȃ�
1106             M_Out(12346) = 1 Dly 0.5        ' �g���J�n����M
1107             Mvs PTicketRead
1108             MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
1109 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1110 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1111             If MRtn = 1 Then M_22# = MAssyOK%
1112             If M_20# = MContinue% Then M_22# = MContinue%
1113             If M_20# = MPass% Then M_22# = MPass%
1114             If M_20# = MNext% Then M_22# = MPass%
1115         EndIf
1116     EndIf
1117     If M_20# = MNgProcess% Or M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1118     M_20# = MClear%
1119     Mov PTicketRead_1
1120     Mov PTicketRead_2
1121     Mov PBracketFSet_3
1122 '
1123 '�u���ʒu�摜����
1124 'Wait M_In(11893) = 1         '�˂����{3��~6�܂őҋ@
1125 MRtn = fScrewTighenRoboCheck(11893)    '��~��Ԃ���M����
1126 If MRtn = 0 Then Mov PInitialPosition1  '��~�ʒu�Ɉړ�
1127 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1128 '
1129 If M_In(11369) = 0 Then M_Out(12912) = 1    '�摜���薢�g�p�������Ńt���O����6/23����
1130 If M_In(11369) = 0 Then GoTo *SkipCheck1    '�摜���薢�g�p���W�����v
1131 *RE_FLG_SET_1
1132 'Wait M_In(11920) = 0                'BaseUnit5���u����t���O�m�F(�ǉ�2/27����)
1133 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5���u����t���O�m�F(�^�C���A�E�g������ǉ�22/09/29����)
1134 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1135 If MRtn = 2 Then GoTo *RE_FLG_SET_1
1136 M_Out(12912) = 1                    '���u����t���O����
1137 'Wait M_In(11920) = 0              'Ver 0.4 �ǉ�
1138 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5���u����t���O�m�F(�^�C���A�E�g������ǉ�22/09/29����)
1139 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1140 If MRtn = 2 Then GoTo *RE_FLG_SET_1
1141 Dly 0.3
1142 'If M_In(11920) = 1 Then M_Out(12912) = 0     'Ver 0.4 �R�����g�A�E�g�@�H��6�D��
1143 If M_In(11920) = 1 Then GoTo *RE_FLG_SET_1   '
1144 '
1145 'Wait M_In(11920) = 0              'BaseUnit5���u����t���O�m�F(�ǉ���������10/1����)
1146 ''
1147 'M_Out(12912) = 1                  '���u����t���O����(�Փ˖h�~)
1148 '
1149 'Mov PBracketFCheck_2         '�o�H
1150 'Mov PBracketFCheck           '�����ʒu
1151 *RE_F_CHECK
1152 'If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck2
1153 PInspPosition(1) = PBracketFCheck1
1154 MInspGroup%(1) = 4
1155 PInspPosition(2) = PBracketFCheck2
1156 MInspGroup%(2) = 5
1157 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1158 If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck1
1159 If MRtn = 0 Then
1160     MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1161 EndIf
1162 If MRtn = 1 Then GoTo *CompFCheck
1163 fErrorProcess(11,43,46,0)
1164 If M_20# = MNext% Then M_20# = MClear%
1165 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1166     Mov PBracketFCheck
1167     Mov PInitialPosition
1168     M_Out(12912) = 0
1169 EndIf
1170 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1171 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1172 If M_20# = MContinue% Then GoTo *RE_F_CHECK
1173 *CompFCheck
1174 *SkipCheck1
1175 '
1176 '
1177 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~6�`��~7�܂�)
1178 If M_In(11369) = 1 Then Mov PBracketRCheck1
1179 'Wait M_In(11894) = 1         '�˂����{3��~7�܂őҋ@
1180 MRtn = fScrewTighenRoboCheck(11894)    '��~��Ԃ���M����
1181 If MRtn = 0 Then
1182     Mov PBracketFCheck
1183     Mov PInitialPosition
1184     M_Out(12912) = 0
1185 EndIf
1186 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1187 '
1188 If M_In(11369) = 0 Then GoTo *SkipCheck2    '�摜���薢�g�p���W�����v
1189 *RE_R_CHECK
1190 PInspPosition(1) = PBracketRCheck1
1191 MInspGroup%(1) = 2
1192 PInspPosition(2) = PBracketRCheck2
1193 MInspGroup%(2) = 3
1194 MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1195 If M_In(MIN_Insight_Use%) = 0 Then GoTo *SkipCheck2
1196 If MRtn = 0 Then
1197     MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,2,-1,1)
1198 EndIf
1199 If MRtn = 1 Then GoTo *CompRCheck
1200 fErrorProcess(11,43,46,0)
1201 If M_20# = MNext% Then M_20# = MClear%
1202 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1203     Mov PBracketFCheck
1204     Mov PInitialPosition
1205     M_Out(12912) = 0
1206 EndIf
1207 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1208 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1209 If M_20# = MContinue% Then GoTo *RE_R_CHECK
1210 *CompRCheck
1211 *SkipCheck2
1212 '
1213 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~7�`��~8�܂�)�����ʒu�ύX1/20����
1214 '
1215 If M_In(11369) = 1 Then Mov PBracketFCheck
1216 '
1217 '
1218 '�˂����{3��DVDassy�����
1219 'M_Out(12866) = 1 Dly 0.5                   '�˂����{3�ɍĊJ�v��(��~7�`��~8�܂�)�����ʒu�ύX1/20����
1220 Mov PMechaOnRoboGet_3                       '�������킹
1221 'M_Out(12912) = 0                            '���u����t���O���(6/23�b��R�����g�A�E�g(����))
1222 '
1223 Mov PMechaOnRoboGet_2                       '���_(�����ʒu�ύX1/20����)
1224 '
1225 'Wait M_In(11895) = 1                       '�˂����{3��~8�܂őҋ@
1226 MRtn = fScrewTighenRoboCheck(11895)         '��~��Ԃ���M����
1227 If MRtn = 0 Then Mov PInitialPosition1      '��~�ʒu�Ɉړ�
1228 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1229 '
1230 *RE_FLG_SET_2
1231     fnAutoScreenComment(533)    '��ԕ\��[�H���T�̃��{����I���҂�] 2022/04/26 �n��
1232 'Wait M_In(11920) = 0                        'BaseUnit5���u����t���O�m�F(�����ǉ�2/9����)
1233 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5���u����t���O�m�F(�^�C���A�E�g������ǉ�22/09/29����)
1234 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1235 If MRtn = 2 Then GoTo *RE_FLG_SET_2
1236 '
1237 M_Out(12912) = 1                            '���u����t���O����(�Փ˖h�~)
1238 Dly 0.3
1239 'Wait M_In(11920) = 0                        'Ver 0.4 �ǉ��@�H��6�D��
1240 MRtn = fTimeOutJudge(11920,0)    'BaseUnit5���u����t���O�m�F(�^�C���A�E�g������ǉ�22/09/29����)
1241 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1242 If MRtn = 2 Then GoTo *RE_FLG_SET_2
1243 'If M_In(11920) = 1 Then M_Out(12912) = 0   'Ver 0.4 �R�����g�A�E�g
1244 If M_In(11920) = 1 Then GoTo *RE_FLG_SET_2
1245 '
1246 'Mov PMechaOnRoboGet_2                      '�����ʒu�ύX(1/20����)
1247 Mov PMechaOnRoboGet_1                       '�˂����{���
1248 Ovrd 25
1249 '
1250 *RE_ROBO_GET_1
1251 '
1252     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1253 Mvs PMechaOnRoboGet          'DVD���J�󂯎��ʒu
1254 Dly 0.1
1255 M_Out(12257) = 0             'DVD�`���b�N�JOFF
1256 M_Out(12256) = 1             'DVD�`���b�N��ON
1257 '
1258 'Wait M_In(11266) = 1         'DVD���J�`���b�N���o
1259 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1260 If MRtn = 1 Then GoTo *CompRoboGet1
1261 fErrorProcess(11,269,284,0)
1262 If M_20# = MNext% Then M_20# = MClear%
1263 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1264 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1265 If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1266 *CompRoboGet1
1267 '
1268 M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~8�`��~9�܂�)
1269 '
1270 'Wait M_In(11876) = 1         '�˂����{3�˂����ߊ�������M
1271 '
1272 'Wait M_In(11896) = 1         '�˂����{3��~9�܂őҋ@
1273 MRtn = fScrewTighenRoboCheck(11876)    '��~��Ԃ���M����
1274 'If MRtn = 0 Then
1275 '    M_Out(12256) = 0             'DVD���J�`���b�N��OFF
1276 '    M_Out(12257) = 1             'DVD���J�`���b�N�JON
1277 '    Mvs PMechaOnRoboGet_1
1278 '    Mov PMechaOnRoboGet_2
1279 '    Mov PInitialPosition
1280 'EndIf
1281 If MRtn = 0 Then GoTo *ASSY_ERROR_END   '���̏�Œ�~
1282 '
1283 'Wait M_In(11264) = 1         'DVD���J���o
1284 *RE_ROBO_GET_2
1285 MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   'DVD���J���o
1286 If MRtn = 1 Then GoTo *CompRoboGet2
1287 fErrorProcess(11,273,284,0)
1288 If M_20# = MNext% Then M_20# = MClear%
1289 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1290 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1291 If M_20# = MContinue% Then GoTo *RE_ROBO_GET_2
1292 *CompRoboGet2
1293 '
1294 Mvs PMechaOnRoboGet_1        '�˂����{���
1295 Ovrd 100
1296 Mov PMechaOnRoboGet_2        '�˂����{���_
1297 'M_Out(12866) = 1 Dly 0.5     '�˂����{3�ɍĊJ�v��(��~9�`�˂����ߊ����܂�)
1298 M_Out(12912) = 0                  '���u����t���O���(�ǉ�10/1����)
1299 '
1300 Mov PTicketRead_2
1301 M_Out(12868) = 1 Dly 0.5     '�˂����{3�˂����ߊ����𑗐M(�����ʒu�ύX3/26����)
1302 '
1303     If M_22# = MAssyOK% Then GoTo *CompRead
1304 *IRREGULAR      '�J�n��Assy������DVD���J�c���W�����v��
1305     Mov PTicketRead_1
1306 '
1307 'DVDassy���p���b�g�֒u��
1308 '    Wait M_In(11218) = 1         '�p���b�g���㏸���Ă��邱�Ƃ��m�F
1309     If M_22# <> MClear% And M_22# <> MIrregular% Then GoTo *RE_PIAS_CHECK
1310     fnAutoScreenComment(95)    '��ԕ\��[�p���b�g�����ҋ@��] 2022/04/26 �n��
1311     Wait M_In(11354) = 1         ' �g���J�n�M�����o�Ă��邱�Ƃ��m�F
1312     M_Out(12346) = 1 Dly 0.5         ' �g���J�n����M
1313     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1314 *RE_PIAS_CHECK
1315     M_20# = MClear%                 '������
1316     MRtn = 1
1317     If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
1318         If M_22# = MClear% Or M_22# = MContinue% Or M_22# = MIrregular% Then'PIAS�`�F�b�N�ɂĖ����������g���C���ɓ���
1319                 Mvs PTicketRead
1320                 MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
1321 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1322 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1323         EndIf
1324     EndIf
1325 '
1326     If M_22# = MPass% Then M_20# = MPass%
1327     M_22# = MClear%
1328     If MRtn = 1 And M_20# = MClear% Then GoTo *CompRead
1329 '    fErrorProcess(11,17,0,0)
1330 '    Dly 10                                      '�f�o�b�O�p
1331 '    If M_In(11359) = 1 Then M_22# = MIrregular%'�f�o�b�O�p
1332 '    If M_22# = MIrregular% Then *ASSY_ERROR_END  '�f�o�b�O�p
1333 '    If M_20# = MPass% Then M_20# = MClear%      '�f�o�b�O�p
1334     If M_20# = MPass% Then
1335         M_22# = MIrregular%
1336         M_20# = MAssyOK%
1337         Dly 0.1
1338         Mvs PTicketRead_1                         '22/04/11 �ǉ� �n��
1339     EndIf
1340     If M_20# = MAssyOK% Then GoTo *AssyEnd
1341     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1342     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1343     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1344     If M_20# = MContinue% Then GoTo *RE_PIAS_CHECK
1345 *CompRead
1346 '
1347 'Mov PTicketRead_1
1348 Accel 25 , 25                '�f�o�b�O��
1349 Mov PMechaOnPltSet_2         '�p���b�g���_
1350 Accel 100 , 100
1351 Mov PMechaOnPltSet_1         '�p���b�g���
1352 '
1353 '�u���O��DVD�������Ă��邩�H�m�F    2022/04/11 �n��
1354 'DVD�������Ă����炸�A�`���b�N�̏ꍇ�A�[�̐M����ON���Ȃ�
1355 MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1356 If MRtn = 0 Then                           '�`���b�N�̕[��OFF�̏ꍇ
1357     fErrorProcess(11,269,284,0)
1358     If M_20# = MNext% Then
1359         M_20# = MClear%
1360         GoTo *CompPltSet
1361     EndIf
1362     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1363     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1364     If M_20# = MContinue% Then GoTo *CompRead
1365 EndIf
1366 '
1367 Dly 0.1
1368 Ovrd 10
1369 Mvs PMechaOnPltSet           'DVD���J�u���ꏊ
1370 Dly 0.1
1371 '
1372 *RE_PLT_SET
1373 '
1374 M_Out(12256) = 0             'DVD���J�`���b�N��OFF
1375 M_Out(12257) = 1             'DVD���J�`���b�N�JON
1376 '
1377 'Wait M_In(11265) = 1         'DVD���J�`���b�N�J���o
1378 MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1379 If MRtn = 1 Then GoTo *CompPltSet
1380 fErrorProcess(11,270,284,0)
1381 If M_20# = MNext% Then M_20# = MClear%
1382 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1383 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1384 If M_20# = MContinue% Then GoTo *RE_PLT_SET
1385 *CompPltSet
1386 M_22# = MClear%
1387 '
1388 Ovrd 100
1389 Mvs PMechaOnPltSet_1         '�p���b�g���
1390 'Ovrd 100
1391     MRtn = FnCtlValue2(2)          '�g���n�j�{�P  2022/04/28 �n��
1392 Mov PMechaOnJigGet_3
1393     MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1394 'Mov PInitialPosition   '�R�����g�A�E�g(1/20����)
1395 '
1396 'Wait M_In(11876) = 1         '�˂����{3�˂����ߊ�������M
1397 'MRtn = frInCheck(11876,1,MSETTIMEOUT05&)   '�˂����{3�˂����ߊ�������M�E
1398 'If MRtn = 0 Then
1399 '    fErrorProcess()         '�G���[����
1400 'EndIf
1401 'M_Out(12868) = 1 Dly 0.5     '�˂����{3�˂����ߊ����𑗐M(�����ʒu�ύX3/26����)
1402 '�`�P�b�gID��������
1403 'If M_20# <> MPass% Then M_20# = MAssyOK%
1404 M_20# = MAssyOK%
1405 '
1406 *ASSY_ERROR_END
1407 *AssyEnd
1408 *fnAssyStart_FEndPosi
1409 FEnd
1410 '
1411 '��fnPiasCheck
1412 ''' <summary>
1413 ''' PIAS�`�P�b�g�Ǎ���
1414 ''' </summary>
1415 ''' <returns>   0 : NG
1416 '''             1 : OK(�Ǎ��݊���)
1417 ''' </returns>
1418 ''' <remarks>
1419 ''' Date   : 2021/07/07 : M.Hayakawa
1420 ''' </remarks>'
1421 Function M% fnPiasCheck
1422     fnPiasCheck = 0
1423     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1424     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1425 '
1426 *RETRY_PIAS
1427     M_20# = MClear%
1428     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1429     '
1430     '�yID�`�P�b�g�ǂݍ��݁z
1431     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1432     MInspGroup%(1) = 1              '����G�ԍ�
1433     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1434 '
1435     '�G���[�̏ꍇ
1436     If MRtn <> 1 Then
1437         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1438         If MRtn <> 1 Then
1439             'D720 -> D1300 �R�s�[�v��
1440             M_Out(12565) = 1
1441             Dly 0.5
1442             M_Out(12565) = 0
1443             '�G���[�����L�q
1444             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1445             'GOT KEY���͑҂�
1446             MKeyNumber = fnKEY_WAIT()
1447         '
1448             Select MKeyNumber
1449                 Case MNext%         '���ւ�I�������ꍇ
1450                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1451                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1452 '                    GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��(�֐��O���ŕ��򂷂�悤�ύX3/26����)
1453                     Break
1454                 Case MAbout%        '��~��I�������ꍇ
1455                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1456                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1457 '                    GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��(�֐��O���ŕ��򂷂�悤�ύX3/26����)
1458                     Break
1459                 Case MNgProcess%    'NG��I�������ꍇ
1460                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1461                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1462 '                    GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��(�֐��O���ŕ��򂷂�悤�ύX3/26����)
1463                     Break
1464                 Case MContinue%     '�p����I�������ꍇ
1465                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1466                     M_20# = MContinue%
1467 '                    GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C(�֐��O���ŕ��򂷂�悤�ύX3/26����)
1468                     Break
1469             End Select
1470         EndIf
1471     EndIf
1472     If M_20# <> MClear% Then GoTo *fnPiasCheck_End
1473 '
1474 '----------D720 -> D1300 �R�s�[�v��----------
1475     M_Out(12565) = 1
1476     Dly 0.5
1477     M_Out(12565) = 0
1478 '----------�ʐM�m�F������----------
1479     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1480     MRtn = 0                ' ������
1481     M_20# = MClear%         ' ������
1482     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1483     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j(�֐��O���ŕ��򂷂�悤�ɕύX3/26����)
1484     If MRtn <> 1 Then GoTo *fnPiasCheck_End
1485 '        If M_20# = MContinue% Then
1486 '            GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1487 '        Else
1488 '            GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1489 '        EndIf
1490 '    EndIf
1491 '----------�H�������m�F----------
1492     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1493     MRtn = 0                ' ������
1494     M_20# = MClear%         ' ������
1495     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1496     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j(�֐��O���ŕ��򂷂�悤�ɕύX3/26����)
1497     If MRtn <> 1 Then GoTo *fnPiasCheck_End
1498 '        If M_20# = MContinue% Then
1499 '            GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1500 '        Else
1501 '            GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1502 '        EndIf
1503 '    EndIf
1504     '
1505     fnPiasCheck = 1
1506     *fnPiasCheck_End
1507 FEnd
1508 '
1509 '��fnPCComuCheck
1510 ''' <summary>
1511 ''' PC-PLC�ʐM�`�F�b�N
1512 ''' </summary>
1513 ''' <returns>   0 : NG
1514 '''             1 : OK(�Ǎ��݊���)
1515 ''' </returns>
1516 ''' <remarks>
1517 ''' Date   : 2021/07/07 : M.Hayakawa
1518 ''' </remarks>'
1519 Function M% fnPCComuCheck
1520     fnPCComuCheck = 0
1521     MJudge% = 0                                  '������
1522     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1523     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1524     '
1525     For MStaNo = 0 To 5
1526         '
1527         If M_In(MIN_PIAS_ComOK%) = 1 Then
1528             'PC�ʐMOK(M400)
1529             MJudge% = MOK%
1530             MStaNo = 5
1531             Break
1532         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1533             'toRBT_�ʐM�m�Ftime out
1534             MJudge% = MNG%
1535             MCommentD1001 = 15
1536             MCommentD1002 = 21
1537             MStaNo = 5
1538             Break
1539         Else
1540             'toRBT_�ʐM�m�Ftime out
1541             MJudge% = MNG%
1542             MCommentD1001 = 14
1543             MCommentD1002 = 21
1544             Break
1545         EndIf
1546     Next MStaNo
1547     '
1548     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1549     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1550     '
1551     '�G���[���
1552     If MJudge% <> MOK% Then
1553         M_20# = MClear%     '������
1554         '�G���[�����L�q
1555         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1556         'GOT KEY���͑҂�
1557         MKeyNumber = fnKEY_WAIT()
1558         '
1559         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1560             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1561             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1562             Break
1563         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1564             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1565             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1566             Break
1567         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1568             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1569             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1570             Break
1571         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1572             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1573             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1574             Break
1575         EndIf
1576     Else
1577         'OK�̏ꍇ
1578         fnPCComuCheck = 1
1579     EndIf
1580 FEnd
1581 '
1582 '��fnProcessCheck
1583 ''' <summary>
1584 ''' �H�������m�F
1585 ''' </summary>
1586 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1587 '''             -1�F�O�H������NG  -2�F���H����������
1588 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1589 '''             -5�F���������G���[
1590 ''' </returns>
1591 ''' <remarks>
1592 ''' Date   : 2021/07/07 : M.Hayakawa
1593 ''' </remarks>'
1594 Function M% fnProcessCheck
1595     fnProcessCheck = 0
1596     MJudge% = MNG%      '��UNG���������Ƃ���
1597 '----------�H�������m�F----------
1598     MCommentD1001 = 0   '�R�����g������
1599     For MStaNo = 0 To 5
1600         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1601         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1602         '
1603         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1604             MJudge% = MOK%
1605             fnAutoScreenComment(85)     ' AUTO���
1606             MStaNo = 5
1607             Break
1608         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1609             MFlgLoop% = 0
1610             MJudge% = MNG%
1611             MCommentD1001 = 27
1612             MCommentD1002 = 22
1613             fnAutoScreenComment(94)     ' AUTO���
1614             fnProcessCheck = -2         ' NG��-2��Ԃ�
1615             MStaNo = 5
1616             Break
1617         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1618            MJudge% = MNG%
1619             MCommentD1001 = 31
1620             MCommentD1002 = 22
1621             fnAutoScreenComment(83)     ' AUTO���
1622             fnProcessCheck = -3         ' NG��-3��Ԃ�
1623             MStaNo = 5
1624             Break
1625         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1626             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1627             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1628             MJudge% = MNG%
1629             MCommentD1001 = 32
1630             MCommentD1002 = 22
1631             fnAutoScreenComment(84)     ' AUTO���
1632             fnProcessCheck = -1         ' NG��-1��Ԃ�
1633             Dly 1.0
1634             '�H�������m�FOFF
1635             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1636             Dly 1.0
1637            'MStaNo = 5
1638             Break
1639         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1640             MFlgLoop% = 0
1641             MJudge% = MNG%
1642             MCommentD1001 = 29
1643             MCommentD1002 = 22
1644             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1645             fnProcessCheck = -5         ' NG��-5��Ԃ�
1646             MStaNo = 5
1647             Break
1648         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1649             MJudge% = MNG%
1650             If MCommentD1001 = 32 Then
1651                 '�������Ȃ�
1652             Else
1653                 MCommentD1001 = 26
1654             EndIf
1655             MCommentD1002 = 22
1656             fnProcessCheck = -4         ' NG��-4��Ԃ�
1657             MStaNo = 5
1658             Break
1659         Else
1660             MJudge% = MNG%
1661             MCommentD1001 = 28
1662             MCommentD1002 = 22
1663         EndIf
1664     Next MStaNo
1665     '�H�������m�FOFF
1666     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1667     '�ʉߗ���NG �H�������̏ꍇ
1668     If MJudge% = MPass% Then
1669         M_20# = MPass%
1670     EndIf
1671     '
1672     '�G���[���
1673     If MJudge% <> MOK% Then
1674         M_20# = MClear%     '������
1675         '�G���[�����L�q
1676         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1677         'GOT KEY���͑҂�
1678         MKeyNumber = fnKEY_WAIT()
1679         '
1680         Select MKeyNumber
1681             Case MAbout%        '��~��I�������ꍇ
1682                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1683                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1684                 Break
1685             Case MNext%         '���ւ�I�������ꍇ
1686                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1687                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1688                 Break
1689             Case MContinue%     '�p����I�������ꍇ
1690                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1691                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1692                 Break
1693             Case MNgProcess%    'NG��I�������ꍇ
1694                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1695                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1696                 Break
1697         End Select
1698     Else
1699         fnProcessCheck = 1  ' OK��1��Ԃ�
1700     EndIf
1701 FEnd
1702 '
1703 '��fnPiasWrite
1704 ''' <summary>
1705 ''' Pias �g�����ʏ����ݗv��
1706 ''' </summary>
1707 '''<param name="MFlg%">
1708 '''                 MOK%(1) = �H��������OK��������
1709 '''                 MNG%(0) = �H��������NG��������
1710 '''</param>
1711 '''<returns></returns>
1712 ''' <remarks>
1713 ''' Date   : 2021/07/07 : M.Hayakawa
1714 ''' </remarks>'
1715 Function M% fnPiasWrite(ByVal MFlg%)
1716       fnPiasWrite = 0
1717 *RETRY_PIASWRITE
1718     '
1719     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1720    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1721     If MFlg% = MOK% Then
1722         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1723     Else
1724         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1725     EndIf
1726     Dly 0.1                  '�O�̂���
1727     '
1728     'Pias�֏����݊J�n M305 -> ON
1729     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1730     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1731     '
1732     MJudge% = MNG%
1733     '
1734     For MStaNo = 0 To 5
1735         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1736             MJudge% = MOK%
1737             'MRet = fnAutoScreenComment(85)  'AUTO���
1738             MStaNo = 5
1739             Break
1740         '
1741         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1742             MJudge% = MNG%
1743             'MRet = fnAutoScreenComment(85)  'AUTO���
1744            MCommentD1001 = 34
1745            MCommentD1002 = 25
1746             MStaNo = 5
1747             Break
1748         '
1749         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1750             MJudge% = MNG%
1751             'MRet = fnAutoScreenComment(85)  'AUTO���
1752            MCommentD1001 = 35
1753            MCommentD1002 = 25
1754             MStaNo = 5
1755             Break
1756         '
1757         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1758             MJudge% = MNG%
1759             'MRet = fnAutoScreenComment(85)  'AUTO���
1760            MCommentD1001 = 36
1761            MCommentD1002 = 25
1762             MStaNo = 5
1763             Break
1764         '
1765         Else
1766             MJudge% = MNG%
1767            MCommentD1001 = 42
1768            MCommentD1002 = 25
1769         '
1770         EndIf
1771         '
1772     Next MStaNo
1773     '
1774     'Pias�֏����݊J�n M305 -> OfF
1775     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1776     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1777     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1778     '
1779     '
1780     '�ʉߗ���NG �H�������̏ꍇ
1781     If MJudge% = MPass% Then
1782         M_20# = MPass%
1783     EndIf
1784     '
1785    M_20# = MClear%     '������
1786     '
1787     '�G���[���
1788     If MJudge% < MOK% Then
1789     '
1790 '�c���Ă���������ł͎g�p���Ȃ����x��
1791 *RETRY_ERR_WRITE
1792         M_20# = MClear%     '������
1793         '�G���[�����L�q
1794         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1795         'GOT KEY���͑҂�
1796         MKeyNumber = fnKEY_WAIT()
1797         '
1798         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1799             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1800            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1801             Break
1802         '
1803         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1804             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1805             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1806         '
1807         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1808             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1809             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1810         '
1811         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1812             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1813            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1814             Break
1815         '
1816         EndIf
1817         '
1818         If M_20# = MClear% Then *RETRY_ERR_WRITE
1819         '
1820     EndIf
1821     '
1822     If M_20# = MContinue% Then *RETRY_PIASWRITE
1823     '
1824     fnPiasWrite = 1
1825     '
1826 FEnd
1827 '
1828 '��fnPCBNumberCheck
1829 ''' <summary>
1830 ''' Pias ��ԍ��ƍ��v��
1831 ''' </summary>
1832 '''<param name="%"></param>
1833 '''<param name="%"></param>
1834 '''<returns></returns>
1835 ''' <remarks>
1836 ''' Date   : 2021/07/07 : M.Hayakawa
1837 ''' </remarks>'
1838 Function M% fnPCBNumberCheck
1839       fnPCBNumberCheck = 0
1840     '
1841 *RETRY_PCBCHECK
1842     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1843     'Pias�֊�ƍ��J�n M310 -> ON
1844     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1845     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1846     '
1847     MJudge% = MNG%
1848     '
1849     For MStaNo = 0 To 5
1850         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1851             MJudge% = MOK%
1852             fnAutoScreenComment(96)  'AUTO���
1853             MStaNo = 5
1854             Break
1855         '
1856         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1857             MJudge% = MNG%
1858             fnAutoScreenComment(97)  'AUTO���
1859             MCommentD1001 = 37
1860             MCommentD1002 = 25
1861             MStaNo = 5
1862             Break
1863         '
1864         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1865             MJudge% = MNG%
1866             fnAutoScreenComment(98)  'AUTO���
1867             MCommentD1001 = 38
1868             MCommentD1002 = 25
1869             MStaNo = 5
1870             Break
1871         '
1872         ElseIf M_In(11580) = 1 Then                         'time out
1873             MJudge% = MNG%
1874             fnAutoScreenComment(99)  'AUTO���
1875             MCommentD1001 = 39
1876             MCommentD1002 = 25
1877             MStaNo = 5
1878             Break
1879         '
1880         Else
1881             MJudge% = MNG%
1882            MCommentD1001 = 41
1883            MCommentD1002 = 25
1884         '
1885         EndIf
1886         '
1887     Next MStaNo
1888     '
1889     'Pias�֊�ƍ��J�n M310 -> OfF
1890     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1891     '
1892     '
1893     '�ʉߗ���NG �H�������̏ꍇ
1894     If MJudge% = MPass% Then
1895         M_20# = MPass%
1896     EndIf
1897     '
1898    M_20# = MClear%     '������
1899     '
1900     '�G���[���
1901     If MJudge% < MOK% Then
1902     '
1903 '�c���Ă���������ł͎g�p���Ȃ����x��
1904 *RETRY_ERR_PCBNUMBER
1905         M_20# = MClear%     '������
1906         '�G���[�����L�q
1907         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1908         'GOT KEY���͑҂�
1909         MKeyNumber = fnKEY_WAIT()
1910         '
1911         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1912             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1913             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1914             Break
1915         '
1916         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1917             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1918             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1919         '
1920         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1921             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1922             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1923         '
1924         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1925             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1926             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1927             Break
1928         '
1929         EndIf
1930         '
1931         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1932         '
1933     EndIf
1934     '
1935     If M_20# = MContinue% Then *RETRY_PCBCHECK
1936 FEnd
1937 '
1938 '��ScrewTight_S2
1939 ''' <summary>
1940 ''' �˂����߂��s��
1941 ''' </summary>
1942 '''<param name="PScrewPos()">
1943 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1944 '''             PScrewPos(2)    �F�˂����߉��_
1945 '''             PScrewPos(10)   �F�˂����ߏI������
1946 '''</param>
1947 '''<returns>����
1948 '''         0=�ُ�I���A1=����I��
1949 '''</returns>
1950 ''' <remarks>
1951 ''' Date   : 2021/07/07 : M.Hayakawa
1952 ''' </remarks>'
1953 Function M% ScrewTight_S2(ByVal PScrewPosition())   '�l�W���ߌʐݒ�
1954     ScrewTight_S2 = 0
1955     MOKNGFlg = 0
1956     Ovrd 100
1957     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1958     ' �b��
1959     Ovrd 5
1960     Mvs PScrewPosition(10),-10    ' �p���b�g��˂�����S�@�̏��ֈړ�
1961 '    Ovrd MOvrdA
1962     '�b��}�X�N
1963 '    M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
1964 '    Dly 0.1
1965 '    M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1966 '    'Spd 8.3 '�O�����C�h100  �������C�h60   '���C�h100-40�@100%�FSpd�@15�@'�˂����ߑ��x�ݒ�
1967 '    Spd MSpdA               '�l�W���ߎ�Spd�ʐݒ�
1968     ' �b��ړ��̂�
1969     Mvs PScrewPosition(10)
1970 '    '
1971 '    Dly 0.1
1972 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1973 '    Wait M_In(11584)=1          '����/�G���[���o
1974 '    Dly 0.1
1975 '    Spd M_NSpd
1976 '    '
1977 '    If M_In(X28_Driver)=1 Then  '�˂��g�[�^���G���[���o��
1978 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1979 '        Dly 0.1
1980 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1981 '        Dly 0.1
1982 '        M_Out(Y63_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1983 '        Dly 0.1
1984 '        M_Out(Y65_Driver)=0     '�v���O���������@F1
1985 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1986 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
1987 '        MOKNGFlg = -1
1988 '        ScrewTight_S2 = 0
1989 '    Else
1990 '        Wait M_In(X29_Driver)=1 ' ���튮����
1991 '        Dly 0.1
1992 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1993 '        Dly 0.1
1994 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O����
1995 '        Dly 0.1
1996 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
1997 '        Dly 0.1
1998 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1999 '        ScrewTight_S2 = 1
2000 '    EndIf
2001 ' �b��
2002     Ovrd 10
2003     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
2004     Ovrd 100
2005 FEnd
2006 '
2007 '��ScrewGet_S3
2008 ''' <summary>
2009 ''' �˂������@����˂��𓾂�
2010 ''' </summary>
2011 '''<param name="%"></param>
2012 '''         PScrewPos(1)    �F�˂�������̂˂����
2013 '''         PScrewPos(2)    �F�˂���������_
2014 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2015 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
2016 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
2017 '''<returns>����
2018 '''         0=�ُ�I���A1=����I���A-1=M�l�W�Z���T�[NG�A-2=M�l�W�Z���T�[ON�A-3=�z���G���[
2019 '''</returns>
2020 ''' <remarks>
2021 ''' Date   : 2021/07/07 : M.Hayakawa
2022 ''' </remarks>'
2023 Function M% ScrewGet_S3(ByVal PScrewPosition())
2024     ScrewGet_S3 = 0
2025     MMScrewJudge% = 0
2026     '�˂������평������G���[�`�F�b�N
2027 ' ���b��폜
2028 '    Wait M_In(X34_ScrewReady1)=1 '�˂�������S��Ready�ɂȂ�܂ő҂@���@���b���҂���Ready�ɂȂ�Ȃ���Δ�����v���O�������K�v�H
2029 '    Ovrd 100
2030 '    If M_In(X33_SS2)=0 Then  'M�˂����o�Z���T��OFF�i�̏�j���Ă����ꍇ
2031 '        Ovrd 30
2032 '        Mvs,-80             '���̏ꏊ����80mm���ֈړ�
2033 '        Mov PInitPos19049   '19049�����ʒu�ֈړ�
2034 '        M_Out(Y6A_VV1)=0    '�˂��z�� Off
2035 '        'NG�Ƃ��Ă����̊֐����甲����
2036 '        ScrewGet_S3 = -1
2037 '        MMScrewJudge% = 1
2038 '        MCommentD1001 = 61
2039 '    EndIf
2040 '    If ScrewGet_S3 = 0 Then
2041 '        'S�^�C�g�p�˂������@��M�˂����������Ă��Ȃ����Ď�
2042 '        MMScrewJudge% = 0 'MMScrewJudge������������
2043 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT05&)
2044 '        If MRtn = 0 Then
2045 '            Ovrd 30
2046 '            Mvs,-80            '���̏ꏊ����50mm���ֈړ�
2047 '            Mov PInitPos19049  '19049�����ʒu�ֈړ�
2048 '            MMScrewJudge% = 2
2049 '            MRtn = All_CLamp_Release()'�S�ẴN�����v�����֕���
2050 '            MCnt% = 2   '2��ݒ�
2051 '            MCommentD1001 = 62
2052 '        EndIf
2053 '        If MMScrewJudge% = 2 Then
2054 '            ScrewGet_S3 = -2
2055 '        EndIf
2056 '    EndIf
2057 '    'M�l�W���肪ON�̏ꍇ NG�Ƃ��Ċ֐��𔲂���
2058 '    If MMScrewJudge% = 2 Then
2059 '        ScrewGet_S3 = -2
2060 '    EndIf
2061     'S�l�W�p�˂����Y��M�l�W�����m�F�p�����܂�
2062     Ovrd 100
2063     Spd M_NSpd
2064     If MMScrewJudge% = 0 Then
2065         ScrewGet_S3 = 0
2066         M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
2067         MScrewCnt% = 0
2068         MFinCnt% = 2
2069 '        For MCnt% = 0 To MFinCnt%
2070             Mov PScrewPosition(2)        ' �˂������@���_
2071             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
2072             Ovrd 80
2073             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2074             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
2075             Mvs PScrewPosition(10), 1.2
2076             M_Out(Y6A_VV1)=1        ' �˂��z���@ON
2077             '�r�b�g��]
2078             M_Out(Y60_Driver)=1
2079             Dly 0.2
2080             '
2081             Ovrd 100
2082             JOvrd M_NJovrd
2083             Spd M_NSpd
2084             '�l�W�z���m�F�ʒu�ړ�
2085             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
2086             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
2087             '�r�b�g��]��~
2088             'M_Out(Y60_Driver)=0
2089             '
2090             '1�b�ԃl�W�z���m�F
2091 ' �ȉ��b��폜
2092 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2093 '            'MRtn = 0'�����G���[
2094 '            '�z���G���[�̏ꍇ
2095 '            '�l�W���˂����Y�ɖ߂�
2096 '            If MRtn = 0 Then
2097 '                Ovrd 30
2098 '                '�r�b�g��]��~
2099 '                M_Out(Y60_Driver)=0
2100 '                '�l�W�����@���
2101 '                Mvs PScrewPos(1)
2102 '                '�X�ɏ��
2103 '                Mov PScrewPos(1), -75
2104 '                '�l�W�̂Ĉʒu
2105 '                Mov PScrewFeedS021
2106 '                '�z��OFF
2107 '                M_Out(Y6A_VV1)=0 '�˂��z���@OFF
2108 '                Dly 0.2
2109 '                '�j��ON
2110 '                M_Out(Y6B_VB1)=1 '�^��j��ON
2111 '                '�r�b�g��]
2112 '                M_Out(Y61_Driver)=1
2113 '                Dly 0.5
2114 '                '
2115 '                Ovrd 100
2116 '                JOvrd M_NJovrd
2117 '                Spd M_NSpd
2118 '                '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
2119 '                Mov PScrewFeedS021, 10
2120 '                Mov PScrewFeedS021
2121 '                Dly 0.1
2122 '                Mov PScrewFeedS021, 10
2123 '                Mov PScrewFeedS021
2124 '                '
2125 '                '�l�W�����҂�
2126 '                '�r�b�g��]��~
2127 '                M_Out(Y61_Driver)=0
2128 '                Dly 0.1
2129 '                '�j��OFF
2130 '                M_Out(Y6B_VB1)=0 '�^��j��OFF
2131 '                '
2132 '                '
2133 '                '�˂��������Ƃ��āA�ړ��X�ɏ��
2134 '                Mov PScrewPos(1), -75
2135 '                Ovrd 100
2136 '                Spd M_NSpd
2137 '                '�l�W�����@���
2138 '                Mvs PScrewPos(1)
2139 '                '
2140 '                ScrewGet_S3 = -3
2141 '                Break
2142 '                '
2143 '            Else
2144 '                MCnt% = MFinCnt%
2145 '                ScrewGet_S3 = 0
2146 '            EndIf
2147 '        Next  MCnt%
2148         '
2149         Ovrd 100
2150         Spd M_NSpd
2151         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2152         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
2153         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
2154         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2155         '������x�z���m�F
2156 ' �ȉ��b��폜
2157 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2158 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
2159 '            MCommentD1001 = 94
2160 '            MCommentD1002 = 95
2161 '            ScrewGet_S3 = -3
2162 '        EndIf
2163 '        If MRtn = 1 Then      '�z��OK�̏ꍇ
2164 '            ScrewGet_S3 = 1
2165 '        EndIf
2166 '        Break
2167     Else
2168         'M�l�W
2169         If MMScrewJudge% = 2 Then
2170             ScrewGet_S3 = -2
2171         EndIf
2172     EndIf
2173 FEnd
2174 '
2175 '��fnKEY_WAIT()
2176 ''' <summary>
2177 ''' GOT����̃L�[���͑҂�
2178 ''' </summary>
2179 '''<returns>1�F��~    2�F����
2180 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
2181 '''         5�FNG
2182 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
2183 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
2184 '''</returns>
2185 ''' <remarks>
2186 ''' Date   : 2021/07/07 : M.Hayakawa
2187 ''' </remarks>'
2188 Function M% fnKEY_WAIT()
2189     fnKEY_WAIT = 0
2190     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
2191     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
2192     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
2193     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2194     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
2195     Dly 0.2
2196     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
2197     MLocalLoopFlg=1
2198     While MLocalLoopFlg=1
2199         If M_In(11345) = 1 Then         '��~   M5345
2200             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
2201             fnKEY_WAIT = 1
2202             MLocalLoopFlg=-1
2203             Break
2204         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
2205             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
2206             fnKEY_WAIT = 2
2207             MLocalLoopFlg=-1
2208             Break
2209         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
2210             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
2211             fnKEY_WAIT = 3
2212             MLocalLoopFlg=-1
2213             Break
2214         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
2215             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
2216             fnKEY_WAIT = 4
2217             MLocalLoopFlg=-1
2218             Break
2219         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
2220             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
2221             fnKEY_WAIT = 5
2222             MLocalLoopFlg=-1
2223             Break
2224             '
2225         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
2226             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
2227             fnKEY_WAIT = MRobotInit1%
2228             MLocalLoopFlg=-1
2229             Break
2230             '
2231         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
2232             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
2233             fnKEY_WAIT = MRobotInit2%
2234             MLocalLoopFlg=-1
2235             Break
2236             '
2237         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
2238             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
2239             fnKEY_WAIT = MRobotInit3%
2240             MLocalLoopFlg=-1
2241             Break
2242             '
2243         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2244             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2245             fnKEY_WAIT = MRobotInit4%
2246             MLocalLoopFlg=-1
2247             Break
2248             '
2249         Else
2250         EndIf
2251     WEnd
2252     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2253     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2254 FEnd
2255 '
2256 '�� fnAUTO_CTL
2257 ''' <summary>
2258 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2259 ''' </summary>
2260 ''' <remarks>
2261 ''' Date   : 2021/07/07 : M.Hayakawa
2262 ''' </remarks>
2263 Function M% fnAUTO_CTL
2264     fnAUTO_CTL = 0
2265     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2266     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2267     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2268     '
2269     If M_Svo=0 Then             '�T�[�{ON�m�F
2270         Servo On
2271     EndIf
2272     Wait M_Svo=1
2273 FEnd
2274 '
2275 '�� fnWindScreenOpen
2276 ''' <summary>
2277 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2278 ''' </summary>
2279 '''<param name="%"></param>
2280 '''<param name="%"></param>
2281 '''<param name="%"></param>
2282 '''<param name="%"></param>
2283 ''' <remarks>
2284 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2285 ''' MWindReSet = 0     ��ʔ�\��
2286 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2287 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2288 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2289 ''' Date   : 2021/07/07 : M.Hayakawa
2290 ''' </remarks>
2291 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2292     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2293         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2294     EndIf
2295     '
2296     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2297         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2298     EndIf
2299     '
2300     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2301        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2302     EndIf
2303     '
2304     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2305     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2306     Dly 0.5
2307     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2308 FEnd
2309 '
2310 '��FnCtlValue2
2311 ''' <summary>
2312 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2313 ''' </summary>
2314 ''' <param name="MCtlNo%"></param>
2315 ''' <remarks>
2316 ''' Date : 2022/04/28 �n��
2317 ''' </remarks>
2318 '''
2319 '''  1�F������       �{�P
2320 '''  2�F�g���n�j��   �{�P
2321 '''  3�F�g���m�f��   �{�P (���g�p)
2322 '''  4�F�z���G���[�� �{�P
2323 ''' 99�F�Ǐ��J�n�M�� OFF
2324 '''
2325 Function M% FnCtlValue2(ByVal MCtlNo%)
2326     FnCtlValue2 = 1
2327     Select MCtlNo%
2328         Case 1        '�������{�P
2329             M_Out(12569) = 0             '�����݊J�n�M��OFF
2330             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2331             MInputQty = M_In16(11600)    '��������M
2332             MInputQty = MInputQty + 1    '�������{�P
2333             M_Out16(12592) = MInputQty   '���������M
2334             M_Out(12569) = 1             '�����݊J�n�M��ON
2335             Break
2336             '
2337         Case 2        '�g���n�j���{�P
2338             M_Out(12569) = 0             '�����݊J�n�M��OFF
2339             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2340             MAssyOkQty = M_In16(11616)   '�g��OK����M
2341             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2342             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2343             M_Out(12569) = 1             '�����݊J�n�M��ON
2344             Break
2345             '
2346         Case 4        '�z���G���[���{�P
2347             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2348             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2349             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2350             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2351             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2352             M_Out(12569) = 1                       '�����݊J�n�M��ON
2353             Break
2354             '
2355         Case 99        '�Ǐ��J�n�M��OFF
2356             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2357             M_Out(12569) = 0        '�����݊J�n�M��OFF
2358             Break
2359             '
2360     End Select
2361     Exit Function
2362 FEnd
2363 '
2364 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2365 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2366 '-------------------------------------------------------------------------------
2367 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2368 '   ����
2369 '       PInspPos()      �F�����ʒu
2370 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2371 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2372 '       MInspCnt%       �F�����ʒu��
2373 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2374 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2375 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2376 '   �߂�l�F����
2377 '       0=�ُ�I���A1=����I��
2378 '
2379 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2380 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2381 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2382 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2383 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2384 '-------------------------------------------------------------------------------
2385     '----- �����ݒ� -----
2386     Cnt 0                                                           '�ړ�����������(�����l=0)
2387     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2388 '    Cnt 1,0.1,0.1
2389     '�ϐ��錾�E������
2390     Def Inte MNum                                                   '�����ԍ�(������1�`)
2391     MNum% = 1                                                       '�����ԍ������l�ݒ�
2392     Def Inte MEndFlg                                                '�����I���t���O
2393     MEndFlg% = 0
2394     '
2395     '����G�ԍ��ݒ�v���E�������s�v��off
2396     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2397     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2398     '�G���[�ԍ��N���A
2399     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2400     M_Out16(MOUT_InspErrNum) = MInspErrNum
2401     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2402     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2403     '
2404     'Insight Ready check?
2405     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2406         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2407         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2408         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2409         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2410         Exit Function
2411     EndIf
2412     '
2413     '�����ʒu���m�F
2414     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2415         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2416         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2417         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2418         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2419         Exit Function
2420     EndIf
2421     '
2422     '
2423     '
2424     '----- ���C������ -----
2425     '�ݒ肳�ꂽ�����ʒu�����̌������s
2426     While( MEndFlg% = 0 )
2427         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2428         MSetGrNumRetryExitFlg = 0
2429         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2430         While( MSetGrNumRetryExitFlg = 0 )
2431         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2432             '
2433             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2434             '
2435             '----- �����O���[�v�ԍ��ݒ� -----
2436             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2437             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2438             '
2439             '�����ʒu�ֈړ��E�ړ������҂�
2440             Mvs PInspPos( MNum% )                                       '�ړ�
2441             Dly 0.05                                                    '�ړ�������Delay
2442             '
2443             '�����O���[�v�ԍ��ݒ�I���m�F
2444             M_Timer(1) = 0
2445             MExitFlg = 0
2446             While( MExitFlg = 0 )
2447                 '����G�ݒ萳��I��?
2448                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2449                     MExitFlg = 1
2450                 '
2451                 '����G�ݒ�ُ�I��?
2452                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2453                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2454                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2455                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2456                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2457                     EndIf
2458                     MExitFlg = 1
2459                 '
2460                 'timeout�`�F�b�N
2461                 ElseIf 1000 < M_Timer(1) Then
2462                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2463                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2464                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2465                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2466                     EndIf
2467                     MExitFlg = 1
2468                 EndIf
2469             WEnd
2470             '
2471             '����G�ԍ��ݒ�v��off
2472             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2473             '
2474             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2475             'NG�Ȃ���Δ�����
2476             If MCurrentStepErr = 0 Then
2477                 MSetGrNumRetryExitFlg = 1
2478             Else
2479                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2480                 If MSetGrNumRetryCnt = 0 Then
2481                     MSetGrNumRetryExitFlg = 1
2482                 Else
2483                     'Retry�ց@���̑O��Delay
2484                     Dly 0.5
2485                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2486                 EndIf
2487             EndIf
2488             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2489             '
2490         WEnd
2491         '
2492         '
2493         '
2494         '----- �������s -----
2495         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2496             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2497                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2498                 MInspRetryExitFlg = 0
2499                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2500                 While( MInspRetryExitFlg = 0 )
2501                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2502                     '
2503                     '���������m�F
2504                     MRetryCnt = MRetryCnt - 1
2505                     M_Timer(1) = 0
2506                     MExitFlg = 0
2507                     While( MExitFlg = 0 )
2508                     '���������҂�
2509                         '����OK�I��?
2510                         If M_In( MIN_IS_InspOK% ) = 1  Then
2511                             MJudgeOKFlg = 1                         '����OK�t���OON
2512                             MExitFlg = 1
2513                         '
2514                         '����NG�I��?
2515                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2516                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2517                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2518                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2519                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2520                                 EndIf
2521                             EndIf
2522                             MExitFlg = 1
2523                         '
2524                         '�����ُ�I��(IS timeout)?
2525                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2526                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2527                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2528                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2529                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2530                                 EndIf
2531                             EndIf
2532                             MExitFlg = 1
2533                         '
2534                         'timeout�`�F�b�N
2535                         ElseIf 3000 < M_Timer(1) Then
2536                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2537                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2538                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2539                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2540                                 EndIf
2541                             EndIf
2542                             MExitFlg = 1
2543                         EndIf
2544                     WEnd
2545                     '
2546                     '�����J�n�v��off
2547                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2548                     '
2549                     'OK�Ȃ甲����
2550                     If MJudgeOKFlg = 1 Then
2551                         MInspRetryExitFlg = 1
2552                     Else
2553                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2554                         If MRetryCnt = 0 Then
2555                             MInspRetryExitFlg = 1
2556                         Else
2557                             'Retry�ց@���̑O��Delay
2558                             Dly 0.3
2559                         EndIf
2560                     EndIf
2561                     '
2562                 WEnd
2563             EndIf
2564         EndIf
2565         '
2566         '
2567         '
2568         MNum% = MNum% + 1                                           '����Step+1
2569         '�����I���m�F�@�����I���t���O�Z�b�g
2570         If (MInspCnt% < MNum% ) Then
2571             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2572         EndIf
2573         'NG���������s������
2574         If MInspErrNum <> 0 Then                                    'NG����?
2575             If MNgContinue% <> 1 Then                               'NG���s?
2576                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2577             EndIf
2578         EndIf
2579     WEnd
2580     '
2581     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2582     If 0 < MZAxis% Then
2583         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2584         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2585         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2586     EndIf
2587     '
2588     '�߂�l�ݒ�
2589     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
2590         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2591     Else
2592         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2593         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2594         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2595     EndIf
2596     Fine 0 , P
2597     '
2598 FEnd
2599 '
2600 ' ��ISInspection
2601 ''' <summary>
2602 ''' Insight�ɂ��摜�����������s
2603 ''' </summary>
2604 '''<param name="PInspPos()">�����ʒu</param>
2605 '''<param name="MInspGrNum%()">�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j</param>
2606 '''             PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2607 '''<param name="MInspCnt%">�����ʒu��</param>
2608 '''<param name="MZAxis%">�I������Z���ޔ����W�i-1:�����j</param>
2609 '''             �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2610 '''<param name="MNgContinue%">=1�Ō����G���[�ENG�������ɑSStep�̌������s��</param>
2611 '''<returns>    ���� 0=�ُ�I���A1=����I��</returns>
2612 '''         MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2613 '''         MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���"
2614 ''' <remarks>
2615 ''' Date   : 2021/07/07 : M.Hayakawa
2616 ''' </remarks>
2617 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2618 '    '�摜�g�p�m�F 0<- �摜�m�F�����̏ꍇ
2619 '    If M_In(11369) = 0 Then            'toRBT_�g�p�m�F
2620 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2621 '    EndIf
2622 ''
2623 '    Cnt 0                                                       '�ړ�����������(�����l=0)
2624 '    Fine 0.05,P                                                 '�ʒu���ߊ��������ݒu�@0.05mm
2625 '    MNum% = 1                                                   '�����ԍ������l�ݒ�
2626 '    Def Inte MEndFlg                                            '�����I���t���O
2627 '    MEndFlg% = 0
2628 '    '
2629 '    '�G���[�ԍ��N���A
2630 '    MInspErrNumSub = 0                                          '�������s�G���[�ԍ�sub
2631 '    MInspErrNum = 0                                             '�������s�G���[�ԍ�
2632 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2633 '    MInspNGStepNum = 0                                          '�������sNGStep�ԍ�
2634 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2635 '    '
2636 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready off�Ȃ�I��
2637 '        MInspErrNum = 20                                        '�������s�G���[�ԍ� 20 Insight offline
2638 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2639 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2640 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2641 ''
2642 '    EndIf
2643 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2644 '    '
2645 '    '�����ʒu���m�F
2646 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2647 '        MInspErrNum = 21                                        '�����f�[�^�Ȃ� 21�@����<1
2648 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2649 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2650 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2651 ''
2652 '    EndIf
2653 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2654 '    '
2655 '    '�ݒ肳�ꂽ�����ʒu�����̌������s
2656 '    While( MEndFlg% = 0 )
2657 '        '�����I���m�F�@�����I���t���O�Z�b�g
2658 '        If (MInspCnt% < MNum% ) Then
2659 '            MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2660 '        EndIf
2661 '        '
2662 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����J�n�@INSPTAST1
2663 '        If MEndFlg% = 0 Then
2664 '            M_01# = MInspGrNum%(MNum%)                          '����G�ԍ����n��
2665 '        EndIf
2666 '        M_02# = MEndFlg%                                        '�����I���t���O���n��
2667 '        M_05# = MNum%                                           '�����ԍ�(������1�`)
2668 '        '�^�X�N�@����G�ݒ�t���O���n��
2669 '        If MEndFlg% = 0 Then
2670 '            If 0 < MInspGrNum%(MNum%) Then
2671 '                M_03# = 1
2672 '            Else
2673 '                M_03# = 0
2674 '            EndIf
2675 '        Else
2676 '            M_03# = 0
2677 '        EndIf
2678 '        '�^�X�N�@�������ʊm�F�t���O���n��
2679 '        If 1 < MNum% Then
2680 '            If 0 < MInspGrNum%(MNum%-1) Then
2681 '                M_04# = 1
2682 '            Else
2683 '                M_04# = 0
2684 '            EndIf
2685 '        Else
2686 '            M_04# = 0
2687 '        EndIf
2688 '        '
2689 '        '�^�X�N�����J�n
2690 '        M_00# = 1                                               'TASK�����J�n
2691 '        '�^�X�N�����J�n�m�F
2692 '        M_Timer(1) = 0
2693 '        MExitFlg = 0
2694 '        While( MExitFlg = 0 )
2695 '            '�����J�n�����m�F
2696 '            If M_00# = 0 And M_10# = 8 Then
2697 '                MExitFlg = 1
2698 '            EndIf
2699 '            'timeout�`�F�b�N
2700 '            If 2000 < M_Timer(1) Then
2701 '                If MNgContinue% = 1 Then                        'NG���s?
2702 '                    MInspErrNumSub = 36                         '�G���[�ԍ��ݒ�36
2703 '                Else
2704 '                    MInspErrNum = 36                            '�G���[�ԍ��ݒ�36
2705 '                EndIf
2706 '                MExitFlg = 1
2707 '            EndIf
2708 '        WEnd
2709 '        '
2710 '        '�����ʒu�ֈړ��E�ړ������҂�
2711 '        If 0 = MInspErrNum Then
2712 '            If MEndFlg% = 0 Then
2713 '                Mvs PInspPos( MNum% )                           '�ړ�
2714 '            EndIf
2715 '        EndIf
2716 '        '
2717 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����I���҂��@INSPTAST1
2718 '        If 0 = MInspErrNum Then
2719 '            M_Timer(1) = 0
2720 '            MExitFlg = 0
2721 '            While( MExitFlg = 0 )
2722 '                '���������҂��i����I���j
2723 '                If M_10# = 1 Then
2724 '                    MExitFlg = 1
2725 '                EndIf
2726 '                '���������҂��i�ُ�I���j
2727 '                If M_10# = 0 Then
2728 '                    If MNgContinue% = 1 Then                    'NG���s?
2729 '                        MInspErrNumSub = M_12#                  '�G���[�ԍ��ݒ�@M12
2730 '                    Else
2731 '                        MInspErrNum = M_12#                     '�G���[�ԍ��ݒ�@M12
2732 '                    EndIf
2733 '                    MExitFlg = 1
2734 '                EndIf
2735 '                'timeout�`�F�b�N
2736 '                If 5000 < M_Timer(1) Then
2737 '                    If MNgContinue% = 1 Then                    'NG���s?
2738 '                        MInspErrNumSub = 31                     '�G���[�ԍ��ݒ�31
2739 '                    Else
2740 '                        MInspErrNum = 31                        '�G���[�ԍ��ݒ�31
2741 '                    EndIf
2742 '                    MExitFlg = 1
2743 '                EndIf
2744 '            WEnd
2745 '        EndIf
2746 '        '
2747 '        '�������ʊm�F
2748 '        If 0 = MInspErrNum Then
2749 '            If 1 < MNum% Then
2750 '                If 0 < MInspGrNum%(MNum%-1) Then                '��������?
2751 '                    If M_11# = 2 Then                           '����NG?
2752 '                        If MNgContinue% = 1 Then                'NG���s?
2753 '                            If MInspNGStepNum = 0 Then          'NG������?
2754 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2755 '                            EndIf
2756 '                            MInspErrNumSub = 32                 '�G���[�ԍ��ݒ� 32:����NG
2757 '                        Else
2758 ''                            MInspNGStepNum = MNum% - 1          '�������sNGStep�ԍ��ݒ�
2759 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2760 '                            MInspErrNum = 32                    '�G���[�ԍ��ݒ� 32:����NG
2761 '                        EndIf
2762 '                   EndIf
2763 '                EndIf
2764 '            EndIf
2765 '        EndIf
2766 '        '
2767 '        '�G���[�Ȃ猟�����f�I������̂�Loop���甲���邽�ߏI���t���O�Z�b�g
2768 '        If 0 <> MInspErrNum Then
2769 '            MEndFlg% = 1
2770 '        EndIf
2771 '        '
2772 '        '�������s�A�捞�����҂�
2773 '        If 0 = MInspErrNum Then
2774 '            If MEndFlg% = 0 Then
2775 '                If 0 < MInspGrNum%(MNum%) Then                  '��������?
2776 '                    M_Out(MOUT_IS_Insp%) = 1                    '�������s�v��on
2777 '                    '�捞�����m�F
2778 '                    M_Timer(1) = 0
2779 '                    MExitFlg = 0
2780 '                    While( MExitFlg = 0 )
2781 '                        '���������҂�
2782 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2783 '                            MExitFlg = 1
2784 '                        EndIf
2785 '                        'timeout�`�F�b�N
2786 '                        If 2000 < M_Timer(1) Then
2787 '                            If MNgContinue% = 1 Then            'NG���s?
2788 '                                MInspErrNumSub = 33             '�G���[�ԍ��ݒ�33
2789 '                            Else
2790 '                                MInspErrNum = 33                '�G���[�ԍ��ݒ�33
2791 '                            EndIf
2792 '                            MExitFlg = 1
2793 '                        EndIf
2794 '                    WEnd
2795 '                EndIf
2796 '                '
2797 '            EndIf
2798 '        EndIf
2799 '        MNum% = MNum% + 1
2800 '    WEnd
2801 '    '
2802 '    '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2803 '    If 0 < MZAxis% Then
2804 '        PCurrentPos = P_Curr                                    '���݈ʒu�擾
2805 '        PCurrentPos.Z = MZAxis%                                 'Z����ݒ�
2806 '        Mvs PCurrentPos                                         '���݈ʒu���ֈړ�
2807 '    EndIf
2808 '    '
2809 '    'NG���s������
2810 '    If MNgContinue% = 1 Then                                    'NG���s?
2811 '        MInspErrNum = MInspErrNumSub                            '�G���[�ԍ��ݒ�
2812 '    EndIf
2813 '    '
2814 '    '�߂�l�ݒ�
2815 '    If MInspErrNum = 0 Then
2816 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2817 '    Else
2818 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2819 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2820 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2821 '    EndIf
2822 '    '
2823 '*ISInspection_End
2824 'FEnd
2825 '
2826 '��InitialZoneB
2827 ''' <summary>
2828 ''' ����~��̕��A����
2829 ''' 1)���ޔ��@Z������Ɉړ�
2830 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
2831 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
2832 ''' 4)�C�j�V�����|�W�V�����ֈړ�
2833 ''' </summary>
2834 ''' <remarks>
2835 ''' Date : 2022/04/11 : N.Watanabe
2836 ''' </remarks>
2837 Function V fnInitialZoneB()
2838     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/04/26 �n��
2839 '
2840 '�p�����[�^
2841     Ovrd 5
2842 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2843 '    Cmp Pos, &B100011
2844 '
2845 '���A����J�n
2846 '
2847 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������
2848 *RecoveryChuckOpen
2849     PActive = P_Curr          '���݈ʒu���擾
2850     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
2851 'PMechaOnRoboSet(DVD���J�u����)�́A�`���b�N���
2852     If (PActive.X <= PMechaOnRoboSet.X + 1.0) And (PActive.X >= PMechaOnRoboSet.X -1.0) Then
2853         If (PActive.Y <= PMechaOnRoboSet.Y + 1.0) And (PActive.Y >= PMechaOnRoboSet.Y -1.0) Then
2854             If (PActive.Z <= PMechaOnRoboSet.Z + 1.0) And (PActive.Z >= PMechaOnRoboSet.Z -1.0) Then
2855                 MRecoveryChuckOpen = 1
2856             EndIf
2857         EndIf
2858     EndIf
2859 'PMechaOnRoboGet(DVD���J�󂯎��ʒu)�́A�`���b�N���
2860     If (PActive.X <= PMechaOnRoboGet.X + 1.0) And (PActive.X >= PMechaOnRoboGet.X -1.0) Then
2861         If (PActive.Y <= PMechaOnRoboGet.Y + 1.0) And (PActive.Y >= PMechaOnRoboGet.Y -1.0) Then
2862             If (PActive.Z <= PMechaOnRoboGet.Z + 1.0) And (PActive.Z >= PMechaOnRoboGet.Z -1.0) Then
2863                 MRecoveryChuckOpen = 1
2864             EndIf
2865         EndIf
2866     EndIf
2867     If MRecoveryChuckOpen = 1 Then
2868         M_Out(12256) = 0        'DVD�`���b�N��OFF
2869         M_Out(12257) = 1        'DVD�`���b�N�JON
2870         M_20# = 0               'KEY���͏�����
2871         MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
2872         If MRtn = 0 Then
2873             fErrorProcess(11,270,284,0)
2874             If M_20# = MNext% Then M_20# = MClear%
2875             If M_20# = MAbout% Then GoTo *RecoveryEnd
2876             If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2877             If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2878         Else
2879             M_Out(12257) = 0        'DVD�`���b�N�JOFF
2880         EndIf
2881     EndIf
2882 '
2883 '�������@���ځA���ޔ����o���Ȃ����̑Ώ�
2884 '
2885 'PMechaOnRoboSet(Get)�`PMechaOnRoboSet(Get)_1�̃G���A�ɂ���Ƃ��́APMechaOnRoboSet_1��
2886 '�EPMechaOnRoboSet
2887 '�EPMechaOnRoboSet_1
2888 '�EPMechaOnRoboGet
2889 '�EPMechaOnRoboGet_1
2890 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2891     PActive = P_Curr                    '���݈ʒu���擾
2892     If (PActive.X >= -150) And (PActive.X <= -60) Then
2893         If (PActive.Y >= 540) And (PActive.Y <= 570) Then
2894             If (PActive.Z >= 290) And (PActive.Z <= 320) Then
2895                 Mvs PMechaOnRoboSet_1
2896                 Dly 1.0
2897             EndIf
2898         EndIf
2899     EndIf
2900 '
2901 'PMechaOnRoboSet(Get)_1�`PMechaOnRoboSet(Get)_2�̃G���A�ɂ���Ƃ��́APMechaOnRoboSet_2��
2902 '�EPMechaOnRoboSet_1
2903 '�EPMechaOnRoboSet_2
2904 '�EPMechaOnRoboGet_1
2905 '�EPMechaOnRoboGet_2
2906 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2907 '    PActive = P_Curr                    '���݈ʒu���擾
2908 '    If (PActive.X >= -90) And (PActive.X <= 50) Then
2909 '        If (PActive.Y >= 300) And (PActive.Y <= 570) Then
2910 '            If (PActive.Z >= 290) And (PActive.Z <= 430) Then
2911 '                Mvs PMechaOnRoboSet_2
2912 '                Dly 1.0
2913 '            EndIf
2914 '        EndIf
2915 '    EndIf
2916 '
2917 '���ޔ�
2918     PActive = P_Curr
2919     Pmove = PActive
2920     Pmove.Z = 500           '���ޔ�����ꗥ�̍���
2921      If PActive.X < -400 Then
2922         Pmove.Z =290        '����(F)�󂯎��ʒu��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
2923     EndIf
2924     If PActive.X > 400 Then
2925         Pmove.Z =400        '�p���b�g��ɘr��L�΂��Ă���Ƃ���500�܂ŏグ���Ȃ��ׁA��O���u
2926     EndIf
2927     If PActive.Z < Pmove.Z Then
2928         Mvs Pmove
2929     EndIf
2930     Dly 1.0
2931 'J1���ȊO��ޔ��|�W�V�����ֈړ�
2932     JActive = J_Curr
2933     Jmove = JTaihi
2934     Jmove.J1 = JActive.J1        'J1���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
2935     Jmove.J6 = JActive.J6        'J6���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
2936     Mov Jmove
2937     Dly 1.0
2938 'J1���݂̂�ޔ��|�W�V�����ֈړ�
2939     Mov JTaihi
2940     Dly 1.0
2941 '�C�j�V�����|�W�V�����ֈړ�
2942     Mov PInitialPosition
2943     Cmp Off
2944     Ovrd 100
2945 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n         '2022/04/20 �t�@���N�V�����̊O�ֈړ� �n��
2946 '    If M_In(11856) = 0 Then                 ' ��~���̂�
2947 '        M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
2948 '        MRet = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
2949 '        If MRet = 0 Then
2950 '        Else
2951 '            M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
2952 '        EndIf
2953 '    EndIf
2954     fErrorProcess(11,253,281,0)
2955     Exit Function
2956 *RecoveryEnd
2957 FEnd
2958 '
2959 '
2960 '��fnAutoScreenComment
2961 ''' <summary>
2962 ''' ���C����ʂ̓���󋵕\��
2963 ''' �R�����gD1005�̐ݒ�
2964 ''' </summary>
2965 '''<param name="McommentD1005%">�R�����gID</param>
2966 ''' <remarks>
2967 ''' Date   : 2021/07/07 : M.Hayakawa
2968 ''' </remarks>
2969 Function fnAutoScreenComment(ByVal McommentD1005%)
2970     M_Out16(12576) = McommentD1005%
2971 FEnd
2972 '
2973 '��fnRoboPosChk
2974 ''' <summary>
2975 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2976 ''' </summary>
2977 '''<param name="MINNumber%">���͔ԍ�</param>
2978 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2979 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2980 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2981 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2982 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2983 ''' <remarks>
2984 ''' Date   : 2021/07/07 : M.Hayakawa
2985 ''' </remarks>
2986 Function M% fnRoboPosChk
2987     fnRoboPosChk = 0
2988     MRet = fnStepRead()
2989     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2990     '�E�B���h��ʐ؊���
2991     If MRBTOpeGroupNo > 5 Then
2992         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2993         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2994         Dly 0.2
2995         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2996         Dly 1.5
2997         '
2998         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2999         '
3000         MLoopFlg% = 1
3001         While MLoopFlg% = 1
3002             '
3003             '
3004             MKeyNumber% = fnKEY_WAIT()
3005             Select MKeyNumber%
3006                 Case Is = MAbout%       '��~
3007                     M_20# = MAbout%
3008                     MLoopFlg% = -1
3009                     Break
3010                 Case Is = MNext%        '����
3011                     'MLoopFlg% = -1
3012                     Break
3013                 Case Is = MContinue%    '�p��
3014                     M_20# = MContinue%
3015                     MLoopFlg% = -1
3016                     Break
3017                 Default
3018                     Break
3019             End Select
3020         WEnd
3021     EndIf
3022     '
3023     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
3024         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3025         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
3026         Select MRBTOpeGroupNo
3027             Case Is = 5                          '�������Ȃ�
3028                 Break
3029             Case Is = 10                         '�����ʒu�֖߂�
3030                 'Mov PTEST001
3031                 Break
3032             Case Is = 15                         '�����ʒu�֖߂�
3033                 'Mov PTEST002
3034                 Dly 0.5
3035                 'Mov PTEST001
3036                 Dly 0.5
3037                 Break
3038             Default
3039                 Break
3040         End Select
3041         '
3042         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
3043         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
3044         MRBTOpeGroupNo = 5
3045         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
3046         Dly 1.0
3047         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
3048         fnRoboPosChk = 1                        '�����ʒu������s
3049         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3050     EndIf
3051     Exit Function
3052 FEnd
3053 '
3054 '��frInCheck
3055 ''' <summary>
3056 ''' �Z���T�[IN�`�F�b�N
3057 ''' </summary>
3058 '''<param name="MINNumber%">���͔ԍ�</param>
3059 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3060 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3061 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3062 ''' <remarks>
3063 ''' Date   : 2021/07/07 : M.Hayakawa
3064 ''' </remarks>
3065 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3066     M_Timer(4) = 0
3067     MloopFlg = 0
3068     While MloopFlg = 0
3069         MCrtTime& = M_Timer(4)
3070         If M_In(MINNumber%) = MCMPFLG% Then
3071             MloopFlg = 1
3072             frInCheck = 1
3073         ElseIf MCrtTime& > MTimeCnt& Then
3074             MloopFlg = 1
3075             frInCheck = 0
3076         EndIf
3077     WEnd
3078 FEnd
3079 '-----------------------------------------------
3080 '
3081 '�˂����ߋ@�ʐM�m�F
3082 '
3083 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3084 'fScewTcomChk = 0�@�F����I��
3085 '          �@�@ -1 �F�ُ�I��
3086 '-----------------------------------------------
3087 Function M% fScewTcomChk
3088 *ReCheckScewTcomChk
3089     fScewTcomChk = 0
3090     '�ʐM�m�F���M
3091     M_Out(MOUT_ScwT_ComChk%) = MOn%
3092     '�ʐM�m�F��M�ҋ@
3093 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3094     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3095     '�ʐM�m�F���M�I��
3096     M_Out(MOUT_ScwT_ComChk%) = MOff%
3097     If MRtn = 0 Then
3098         fScewTcomChk = -1
3099     EndIf
3100     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
3101  '
3102 FEnd
3103 '
3104 '
3105 '-----------------------------------------------
3106 '
3107 '�˂����ߊJ�n���M
3108 '
3109 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3110 'fScewTStart = 0�@�F����I��
3111 '          �@�@-1 �F�ُ�I��
3112 '-----------------------------------------------
3113 Function M% fScewTStart
3114     fScewTStart = 0
3115     nRet% = 0
3116     '�˂����ߊJ�n�ҋ@����M
3117 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3118     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3119     If MRtn = 0 Then nRet% = -1
3120     If MRtn = 0 Then GoTo *ScrewStartERROR      '�J�n�ł��Ȃ������ꍇ�W�����v
3121     Dly 0.1
3122     '�˂����ߊJ�n��M�𑗐M
3123     M_Out(MOUT_ScwT_ST%) = MOn%
3124     Dly 0.5
3125     'Wait M_In(MTEST_KEY%) = MOn%
3126     '�˂����ߊJ�n���M�I��
3127     M_Out(MOUT_ScwT_ST%) = MOff%
3128     '
3129 *ScrewStartERROR
3130     fScewTStart = nRet%
3131 FEnd
3132 '
3133 '
3134 '
3135 '-----------------------------------------------
3136 '
3137 '�˂����ߊ�����M
3138 '
3139 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3140 'fScewTcomChk = 0�@�F����I��
3141 '          �@ �@-1 �F�ُ�I��
3142 '-----------------------------------------------
3143 Function M% fScewTFinish
3144 *ReCheckScewTFinish
3145     fScewTFinish = 0
3146     '�˂����ߊ����ҋ@����M
3147 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3148     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3149     If MRtn = 0 Then
3150         fScewTFinish = -1
3151     EndIf
3152     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3153     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3154     Dly 0.1
3155     '�˂����ߊ�����M�𑗐M
3156     M_Out(MOUT_ScwT_FinOK%) = MOn%
3157     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3158     '�˂����ߊJ�n���M�I��
3159     M_Out(MOUT_ScwT_FinOK%) = MOff%
3160     'Wait M_In(MTEST_KEY%) = MOn%
3161     '
3162 *ScewTFinish_ErrEnd
3163 FEnd
3164 '
3165 '
3166 '-----------------------------------------------
3167 '
3168 '����xx��~��M
3169 '
3170 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3171 'fScewTCaseStop = 0�@�F����I��
3172 '          �@   �@-1 �F�ُ�I��
3173 '-----------------------------------------------
3174 Function M% fScewTCaseStop(ByVal MCase%())
3175 *ReCheckScewTCaseStop
3176     fScewTCaseStop = 0
3177     '����xx��~����M
3178     Wait M_In(MCase%(1)) = MOn%
3179     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3180     If MRtn = 0 Then
3181         fScewTCaseStop = -1
3182     EndIf
3183     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3184     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3185     Dly 0.1
3186     '����xx��~��M�𑗐M
3187     M_Out(MCase%(2)) = MOn%
3188     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3189     '�˂����ߊJ�n���M�I��
3190     M_Out(MCase%(2)) = MOff%
3191 *ScewTCaseStop_ErrEnd
3192     '
3193 FEnd
3194 '
3195 '
3196 '��fScrewTighenRoboCheck
3197 '<summary>
3198 '�˂����{�Ď�
3199 '</summary>
3200 '<param name = "MStopNum%"> ��~�ԍ�</param>
3201 '<returns>���� 0:�˂����{�ُ�I�� 1:OK </returns>
3202 '<make>
3203 '2021/12/2 �����V��
3204 '</make>
3205 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3206     fnAutoScreenComment(503)    '��ԕ\��[�˂����{����I���҂�] 2022/04/26 �n��
3207     fScrewTighenRoboCheck = 1
3208     MScrewTighenRoboFlg% = 1    '�t���O�̏�����
3209     MCheck% = 0
3210     While MScrewTighenRoboFlg% = 1
3211         MCheck% = M_In16(11904)
3212         If M_In(MStopNum%) = 1 Then '��~�ʒu�܂ŗ�����
3213             MScrewTighenRoboFlg% = 0 '�֐��𔲂���
3214             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
3215         EndIf
3216         If MCheck% <> 0 Then
3217             fScrewTighenRoboError(MCheck%)
3218             Select M_20#
3219                 Case MAbout%            '��~�������ꂽ�ꍇ
3220                     M_Out(12869) = 1 Dly 1.0
3221                     MScrewTighenRoboFlg% = 0
3222                     fScrewTighenRoboCheck = 0   '�ُ�I��
3223                     Break
3224                 Case MNgProcess%        'NG�������ꂽ�ꍇ
3225                     M_Out(12873) = 1 Dly 1.0
3226                     MScrewTighenRoboFlg% = 0
3227                     fScrewTighenRoboCheck = 0   '�ُ�I��
3228                     Break
3229                 Case MContinue%             '���g���C�������ꂽ�ꍇ
3230                     M_20# = MClear%         'M_20#������
3231                     M_Out(12871) = 1 Dly 1.0
3232                     Break
3233                 Case MNext%                 '���ւ������ꂽ�ꍇ
3234                     M_20# = MClear%         'M_20#������
3235                     M_Out(12874) = 1 Dly 1.0
3236                     Break
3237             End Select
3238             Dly 0.5
3239         EndIf
3240     WEnd
3241 FEnd
3242 '��fScrewTighenRoboError
3243 '<summary>
3244 '�˂����{�G���[����
3245 '</summary>
3246 '<param name = "ErrorCode%"> �G���[�ԍ�</param>
3247 '<make>
3248 '2021/12/2 �����V��
3249 '</make>
3250 Function fScrewTighenRoboError(ErrorCode%)
3251     MCommentD1001 = ErrorCode% + 300
3252     fErrorProcess(11,MCommentD1001,0,0)
3253 FEnd
3254 '
3255 '��fErrorProcess
3256 '<summary>
3257 '�G���[����
3258 '</summary>
3259 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
3260 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
3261 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
3262 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
3263 '<make>
3264 '2021/11/5 �����V��
3265 '</make>
3266 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3267     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
3268     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
3269     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
3270     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
3271     MKeyNum% = 0
3272 *RETRY_ERR_PROCESS
3273      M_20# = MClear%     '������
3274 '        '�G���[�����L�q
3275         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3276 '        'GOT KEY���͑҂�
3277         MKeyNum% = fnKEY_WAIT()
3278 '        '
3279         If MKeyNum% = MAbout% Then   '��~��I�������ꍇ
3280             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3281             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3282             Break
3283          '
3284         ElseIf MKeyNum% = MContinue% Then   '�p����I�������ꍇ
3285             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3286             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3287         '
3288         ElseIf MKeyNum% = MNext% Then   '���ւ�I�������ꍇ
3289             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3290             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3291          '
3292         ElseIf MKeyNum% = MNgProcess% Then   '��~��I�������ꍇ
3293             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3294             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3295             Break
3296         '
3297         EndIf
3298         '
3299         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3300 FEnd
3301 '
3302 '��fnTorqueCheck
3303 ''' <summary>
3304 ''' �g���N�`�F�b�N����p�̃��C��
3305 ''' </summary>
3306 ''' <remarks>
3307 ''' Date   : 2021/12/21 : H.AJI
3308 ''' </remarks>'
3309 Function M% fnTorqueCheck
3310     '�g���N�`�F�b�N�����M  �����n��~
3311     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3312     '
3313     fnTorqueCheck = 0
3314     Ovrd 20
3315     Mov PInitialPosition              '�����ʒu�ړ�
3316     Accel 100 , 20
3317     Mvs PHandChange                   '�n���h�����ʒu
3318     Accel 100 , 100
3319     Ovrd 100
3320     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3321     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3322     Dly 0.2
3323     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3324     '
3325     'M6340  �g���N�`�F�b�N��M
3326     'Dly 5.0
3327     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3328     Dly 1.0
3329     M_Out(12340) = 0
3330     '
3331     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3332     M_Out(12835) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3333    Wait M_In(11843) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3334     M_Out(12835) = 0                         '�˂����{�g���N�`�F�b�N��ʐؑ֊���
3335     '
3336     '
3337     MLoopFlg = 1
3338     While MLoopFlg = 1
3339         '
3340 '        Mov PInitialPosition              '�����ʒu�ړ�
3341         '
3342         MKeyNumber = fnKEY_WAIT()
3343         Select MKeyNumber
3344             Case Is = 1           '��~
3345                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3346                 Dly 1.0
3347                 M_Out(12343) = 0
3348                 Ovrd 20
3349                 'Mov PTicketRead_1
3350                 M_Out(12840) = 1          '�g���N�`�F�b�N�I��
3351                 Wait M_In(11859) = 1      '�˂����{����̏I��
3352                 M_Out(12840) = 0          '�g���N�`�F�b�N�I��
3353                 Ovrd 100
3354                 M_20# = 1
3355                 MLoopFlg = -1
3356                 Break
3357             Case Is = 2           '����
3358                 Break
3359             Case Is = 3           '�p��
3360                 Break
3361             Case Is = 4           '�g���N�`�F�b�N�J�n
3362                 M_Out(12342) = 1          '�g���N�`�F�b�N�J�n�v����M M6342
3363                 Dly 1.0
3364                 M_Out(12342) = 0
3365                 If M_In(11862) = 1 Then             '�g���N�`�F�b�J�[�m�F
3366                     fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3367                     MRet = fnScrewMTorque()           '�˂����{�p�g���N�`�F�b�N
3368                 EndIf
3369                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3370                 'MRet = fnMoveTorquePosi()
3371                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3372                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3373                 Break
3374             Default
3375                 Break
3376         End Select
3377     WEnd
3378     '
3379     '�g���N�`�F�b�N����~���M
3380     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3381     '
3382     '���{�b�g�̈ʒu�����ɖ߂�
3383     Mvs PInitialPosition            '�C�j�V�����|�W�V����
3384     '
3385     '
3386  FEnd
3387  '
3388 '
3389 '
3390 '---------------------------
3391 '
3392 '    ���C����ʂ̕\���A��\���ݒ�
3393 '         �R�����gD1001, D1002, D1003�̐ݒ�
3394 '           MWindReSet = 0     ��ʔ�\��
3395 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3396 '           MWindErrScr = 10    �G���[��� D1001, D1002
3397 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3398 '
3399 '---------------------------
3400 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3401     fnMainScreenOpen = 0
3402     '
3403    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3404         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3405     EndIf
3406     '
3407     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3408         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3409     EndIf
3410     '
3411     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3412         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3413     EndIf
3414     '
3415     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3416     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3417     Dly 0.5
3418     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3419 FEnd
3420 '
3421 '��Main
3422 ''' <summary>
3423 ''' �g���N�`�F�b�N������
3424 ''' </summary>
3425 ''' <remarks>
3426 ''' Date   : 2021/12/21 : H.AJI
3427 ''' </remarks>'
3428 Function M% fnScrewMTorque
3429     fnScrewMTorque = 0
3430     M_Out(12838) = 1                         '�g���N�`�F�b�N�J�n1
3431     Wait M_In(11857) = 1                     '��M����
3432     M_Out(12838) = 0                         '�g���N�`�F�b�N�J�n1
3433     Dly 2.0
3434 FEnd
3435 '
3436 '
3437 '----------------------------------------------------------------
3438 'fTimeOutJudge
3439 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3440 '����
3441 'Address% = �Ď��A�h���X�ԍ�
3442 'JudgeFlg% = �ΏۃA�h���X�̐���I�����̒l
3443 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3444 '�߂�l = 0 �G���[
3445 '         1 ����I��
3446 '         2 ���g���C
3447 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3448 '�쐬��
3449 '2022/9/20 ����
3450 '----------------------------------------------------------------
3451 '
3452 Function M% fTimeOutJudge(ByVal MAddress , ByVal MJudgeFlg)
3453     fTimeOutJudge = 0
3454     MJudge% = 1
3455     MRtn = 0
3456     M_20# = MClear%
3457     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3458 *TimeOutLoop
3459     If MRtn = 1 Then GoTo *TimeOut
3460         fErrorProcess(11,202,203,0)
3461         If M_20# = MNext% Then GoTo *TimeOutLoop
3462         If M_20# = MContinue% Then MJudge% = 2
3463         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3464 *TimeOut
3465     fTimeOutJudge = MJudge%
3466 '
3467 *JUDGE_ERROR_END
3468 Exit Function
3469 FEnd
3470 '
3471 '��Main
3472 ''' <summary>
3473 ''' �g���N�`�F�b�N������
3474 ''' </summary>
3475 ''' <remarks>
3476 ''' Date   : 2021/12/21 : H.AJI
3477 ''' </remarks>'
3478 Function M% fnMoveTorquePosi
3479      fnMoveTorquePosi = 0
3480      Ovrd 50
3481     'Mov PTorquePosi000 '�g���N�`�F�b�N����ʒu�ֈړ�
3482      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
3483     'Mov PTorquePosi020 '�g���N�`�F�b�N�r�b�g�W���C���g���
3484     '
3485     '
3486      '�ȉ��͈������񂪍쐬�����g���N�`�F�b�N�v���O����
3487     '
3488     Spd M_NSpd
3489 '-------------      �h���C�o�[RST
3490     M_Out(12240)=0     '�h���C�o�[OFF CCW
3491     M_Out(12241)=0     '�h���C�o�[OFF CW
3492     M_Out(12242)=0     '�h���C�o�[���� C1
3493     M_Out(12243)=0     '�h���C�o�[���� C2
3494     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
3495 '---------------------------------------
3496 '---------------------------------------
3497     Fsc Off            '�͊o�Z���T�@Off  STEP1�͕s�v
3498 '--------------------------------------------------------------
3499 '--------------------------------------------------------------
3500 '[P-11]
3501 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
3502     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
3503    'Mov PTorquePosi020, -10                    ' �g���N-1�@�u���ʒu��� 10mm �ֈړ�
3504     Dly 0.1
3505 '-----------------------
3506    'Cnt 0                           'Cnt����-2�@�I��
3507 '-----------------------
3508     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
3509     Dly 0.2
3510 '-----------------------
3511     M_Out(12242)=1                   '�h���C�o�[�Z�b�g C1
3512     Dly 0.1
3513     M_Out(12243)=1                   '�h���C�o�[�Z�b�g C2 (�o���N3)
3514     Dly 0.1
3515     M_Out(12245)=1                   '�v���O����2�Z�b�g F1  M�l�W
3516     Dly 0.1
3517     'M_Out(12241)=1                   '�h���C�o�[ON  CW
3518    M_Out(12241)=0                   '�h���C�o�[OFF  CW
3519     'Dly 0.1
3520 '--------------------------------
3521     Ovrd 40
3522    'Dly 0.1
3523 '--------------------------------  �l�W���ߑ��x�ݒ�
3524     Spd 14                            '���C�h 100-40 100% :Spd 12
3525     Dly 0.1
3526 '--------------------------------
3527 '--------------------------------
3528 '---------------------------------�y�˂����ߓ���z
3529 '
3530     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
3531    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
3532     Dly 0.3                          '�������҂�
3533    M_Out(12241)=1                   '�h���C�o�[ON  CW
3534 '
3535     Wait M_In(11584)=1                '����/�G���[���o
3536     Dly 0.1
3537     Spd M_NSpd
3538    'Ovrd 20
3539     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
3540     Wait M_In(11257)=1                '�l�W����SC
3541 '---------------------------------
3542     Dly 0.1
3543     M_Out(12241)=0                    '�h���C�o�[OFF CW
3544     Dly 0.1
3545     M_Out(12242)=0                    '�h���C�o�[���� C1
3546     Dly 0.1
3547     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
3548     Dly 0.1
3549     M_Out(12245)=0                    '�v���O����2���� F1
3550 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
3551 '
3552     Mvs PTorqueCheck,-60                       '������mov ����ύX
3553     Dly 0.1
3554 '--------------------------------------------------------------
3555    'Ovrd 80
3556 '--------------------------------------------------------------
3557 '---------------------------------------
3558 '---------------------------------------
3559 '---------------------------------------�G���[���E����
3560    *LBL1
3561    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
3562    Mvs ,-100
3563    M_Out(12241)=0     '�h���C�o�[OFF CW
3564    Dly 0.1
3565    M_Out(12242)=0     '�h���C�o�[���� C1
3566    Dly 0.1
3567    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
3568    Dly 0.1
3569    M_Out(12245)=0     '�v���O�������� F1
3570 '---------------------------------------
3571 '---------------------------------------
3572 '-------------
3573    'Mov PInitPos19049
3574    Dly 0.1
3575 '
3576 '
3577 '
3578 FEnd
3579 '
3580 ''��Main
3581 ''' <summary>
3582 ''' �g������p�̃��C��
3583 ''' </summary>
3584 ''' <remarks>
3585 ''' Date   : 2021/07/07 : M.Hayakawa
3586 ''' </remarks>'
3587 Function Main
3588     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3589     '
3590     If M_Svo=0 Then
3591         Servo On
3592     EndIf
3593     Wait M_Svo=1
3594 '�g���X�^�[�g���t�����v���p���XON (�ʃX���b�g��8����v���ɕύX�j
3595 '    M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3596 '�p�g���C�g����
3597     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3598     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3599     '
3600     M_20# = 0                                   'KEY���͏�����
3601 '    M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����(�ʒu�ړ�1/18����)
3602     MRet% = 0
3603 '�����ʒu�̊m�F�ƈړ�
3604 '
3605 '���A����@���s�E�����s����      2022/03/22 �n�� �쐬
3606     PActive = P_Curr                    '���݈ʒu���擾
3607     MRecoveryPass% = 0
3608     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3609         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3610             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3611                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3612             EndIf
3613         EndIf
3614     EndIf
3615     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3616         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3617             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3618                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3619             EndIf
3620         EndIf
3621     EndIf
3622     If (PActive.X <= PMechaOnJigGet_3.X + 1.0) And (PActive.X >= PMechaOnJigGet_3.X -1.0) Then
3623         If (PActive.Y <= PMechaOnJigGet_3.Y + 1.0) And (PActive.Y >= PMechaOnJigGet_3.Y -1.0) Then
3624             If (PActive.Z <= PMechaOnJigGet_3.Z + 1.0) And (PActive.Z >= PMechaOnJigGet_3.Z -1.0) Then
3625                 MRecoveryPass% = 1       'DVD���ʒu���ʒu�͕��A����p�X
3626             EndIf
3627         EndIf
3628     EndIf
3629     If MRecoveryPass% = 0 Then
3630        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3631     EndIf
3632 '
3633 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n        'fnInitialZoneB�̊O�ֈړ� 2022/04/20 �n��
3634     If MopeNo <> 2 And M_In(MIN_TorqueCheck%) <> 1 Then       '�g���N�`�F�b�N�̎��͈ȉ������s���Ȃ� 2022/04/21 �n��
3635         If M_In(11856) = 0 Then                 ' ��~���̂�
3636             fnAutoScreenComment(501)            ' ��ԕ\��[�l�W���ߋ@�����^�]�J�n��] 2022/04/25 �n��
3637             M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
3638             MRet% = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
3639             If MRet% = 0 Then
3640             Else
3641                 M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
3642             EndIf
3643         EndIf
3644     EndIf
3645 '
3646 '
3647 '    MRet% = fnRoboPosChk()
3648 '    If MRet% = 1 Then                           '�����ʒu�̓�����s�����ꍇ     '2022/04/20 �R�����g�A�E�g �n��
3649 '        fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  '�E�B���h���
3650 '        MKeyNumber% = fnKEY_WAIT()
3651 '        Select MKeyNumber%
3652 '            Case Is = MAbout%       '��~
3653 '                M_20# = MAbout%
3654 '                MLoopFlg% = -1
3655 '                Break
3656 '            Case Is = MNext%        '����
3657 '                'MLoopFlg = -1
3658 '                Break
3659 '            Case Is = MContinue%    '�p��
3660 '                M_20# = MContinue%
3661 '                MLoopFlg% = -1
3662 '                Break
3663 '            Default
3664 '                Break
3665 '        End Select
3666 '    EndIf
3667     '
3668     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3669         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3670 '�g���N�`�F�b�N
3671         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3672             MRet% = fnTorqueCheck()
3673             Break
3674         Else
3675 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F'12/21�R�����g�A�E�g(����)
3676 '                MRtn = InspInit()               '�摜��������������
3677 '            EndIf
3678             '
3679            M_20# = MClear%                    '������
3680 '�g���J�n
3681             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3682                 'MRet% = fnAssyStart()  '�b��R�����g�A�E�g
3683                 fnAssyStart()
3684             Else
3685                 M_20# = MPass%
3686             EndIf
3687             M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����(�ʒu�ړ�1/18����)
3688 '�g���I�����t����
3689             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3690             Wait M_In(11572) = 1            '���t�擾����
3691             Dly 0.1
3692             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3693 '���t�^�[���j�b�g�ւ�OUT
3694             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3695             fnAutoScreenComment(89)         'AUTO��� �g����������
3696             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3697 'OK/NG�t���O�o��
3698             If M_20# = MAssyOK% Or M_22# = MIrregular% Then
3699                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3700             ElseIf M_20# = MPass% Then
3701                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3702             EndIf
3703 'PIAS�ɑg������������
3704             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3705                 If M_20# = MPass% Then
3706                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3707                 Else
3708                     'KEY���͂�NG�̏ꍇ
3709                     If M_20# = MNgProcess% Then
3710                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3711                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3712                         MRet% = fnPiasWrite(MNG%)
3713                        nAssyNgQty = nAssyNgQty + 1
3714                     EndIf
3715                     '
3716                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(0����MAssyOK%�֕ύX1/12����)
3717                     If M_20# = MAssyOK% Or M_22# = MIrregular% Then
3718                             '-----------------------
3719                             'D732 -> D2600 �R�s�[�v��
3720                             M_Out(12566) = 1
3721 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3722                             M_Out(12566) = 0
3723                             '
3724                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3725                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3726                             '��ԍ��ƍ�(PP�͖��g�p�j
3727 '                            MRet% = fnPCBNumberCheck()
3728                         Else
3729                             MRet% = 1
3730                         EndIf
3731                         '
3732                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3733                             If M_20# <> MAbout% Then
3734                                 '�H������OK��������
3735                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3736                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3737                                 MRet% = fnPiasWrite(MOK%)
3738                                 nAssyOkQty = 0
3739                                 nAssyOkQty = nAssyOkQty + 1
3740                             Else
3741                                 nAssyOkQty = nAssyOkQty + 1
3742                             EndIf
3743                         EndIf
3744                     EndIf
3745 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3746 '                    MRet% = fnPiasWrite(MOK%)
3747                 EndIf
3748             Else
3749                 nAssyOkQty = nAssyOkQty + 1
3750             EndIf
3751             '
3752             '�g���I�����t��������
3753             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3754             '�������A�g��OK���A�g��NG��������
3755 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3756             '
3757 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F'�R�����g�A�E�g12/21(����)
3758 '                '�摜�����I������
3759 '                MRtn = InspQuit()
3760 '            EndIf
3761         EndIf
3762         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3763     EndIf
3764 '�p�g���C�g����
3765     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3766     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3767 'GOT�\��
3768     fnAutoScreenComment(93)  'AUTO��� �H������
3769 FEnd
3770 End
3771 '
3772 '
3773 '���܂��Ȃ��R�����g
3774 '��΍폜�����
3775 '
3776 '
3777 '
3778 '
3779 '
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
PTemp=(+160.71,+268.79,+438.34,-105.40,+88.56,-15.27,+0.00,+0.00)(6,0)
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
PActive=(+160.71,+268.79,+438.34,-105.40,+88.56,-15.27)(6,0)
Pmove=(+302.91,+7.18,+500.00,+180.00,+0.00,-180.00)(7,0)
PBracketFCheck=(-21.45,+489.13,+480.00,-180.00,+0.00,+0.00)(7,0)
PBracketFCheck1=(+2.27,+435.02,+540.18,+145.00,-0.01,-90.00)(7,0)
PBracketFCheck2=(+2.27,+535.02,+540.18,+145.00,-0.01,-90.00)(7,0)
PBracketFCheck_2=(-171.46,+546.61,+480.00,-180.00,+0.00,-90.00)(7,0)
PBracketFCheck_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet=(-605.12,-91.91,+244.44,+179.99,+0.37,+91.79)(7,1)
PBracketFGet_1=(-605.12,-91.91,+269.93,+179.98,+0.37,+91.79)(7,1)
PBracketFGet_2=(-287.41,+0.11,+490.00,-180.00,+0.00,+0.00)(7,0)
PBracketFGet_3=(-177.20,+193.83,+460.54,-179.99,-0.01,-85.26)(7,0)
PBracketFGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet=(-197.54,+547.27,+454.92,-179.60,-0.07,-89.30)(7,0)
PBracketFSet_1=(-197.54,+547.27,+470.00,-179.60,-0.07,-89.30)(7,0)
PBracketFSet_2=(-197.54,+547.27,+540.00,-179.60,-0.07,-89.30)(7,0)
PBracketFSet_3=(-26.65,+244.35,+539.98,-179.99,-0.01,-85.26)(7,0)
PBracketFSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketFSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck=(-21.45,+489.13,+480.00,+180.00,+0.00,+0.00)(7,0)
PBracketRCheck1=(+6.08,+420.00,+537.59,+145.00,+0.00,-90.00)(7,0)
PBracketRCheck2=(+6.08,+550.00,+537.59,+145.00,+0.00,-90.00)(7,0)
PBracketRCheck_2=(-177.57,+546.61,+479.97,-180.00,+0.00,-90.00)(7,0)
PBracketRCheck_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet=(-539.54,-90.81,+244.62,-179.85,-0.06,+92.52)(7,1)
PBracketRGet_1=(-539.54,-90.81,+290.00,-179.85,-0.06,+92.52)(7,1)
PBracketRGet_2=(-287.41,+0.11,+490.00,-180.00,+0.00,+0.00)(7,0)
PBracketRGet_3=(-177.20,+193.83,+460.54,-179.99,-0.01,-85.26)(7,0)
PBracketRGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet=(-152.04,+544.93,+454.54,+179.36,+0.16,-88.84)(7,0)
PBracketRSet_1=(-152.04,+544.93,+460.00,+179.36,+0.16,-88.84)(7,0)
PBracketRSet_2=(-152.04,+544.93,+520.00,+179.36,+0.16,-88.84)(7,0)
PBracketRSet_3=(-26.65,+244.35,+539.98,-179.99,-0.01,-85.26)(7,0)
PBracketRSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PBracketRSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PHandChange=(+347.78,-1.40,+382.90,-90.00,+89.23,-89.99)(6,0)
PInitialPosition=(+303.63,-1.43,+467.78,+165.41,+90.00,+165.42)(6,0)
PInitialPosition1=(+302.91,+7.16,+469.96,-180.00,+0.00,+180.00)(7,0)
PMechaOnJigGet1=(+163.68,+364.78,+175.70,-92.91,+88.79,-2.80)(6,0)
PMechaOnJigGet1_1=(+163.68,+364.78,+200.00,-92.91,+88.79,-2.80)(6,0)
PMechaOnJigGet2=(+163.36,+364.43,+174.78,-73.21,+89.04,+16.52)(6,0)
PMechaOnJigGet2_1=(+163.36,+364.43,+200.00,-73.21,+89.04,+16.52)(6,0)
PMechaOnJigGet_2=(+160.73,+362.85,+350.00,-105.22,+88.56,-15.10)(6,0)
PMechaOnJigGet_3=(+160.71,+268.79,+438.34,-105.40,+88.56,-15.27)(6,0)
PMechaOnJigGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnJigGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnJigGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet=(+317.54,+114.36,+240.37,-41.97,+89.17,-42.02)(6,0)
PMechaOnPltSet_1=(+317.54,+114.36,+280.00,-41.97,+89.17,-42.02)(6,0)
PMechaOnPltSet_2=(+317.54,+114.36,+450.00,-41.97,+89.17,-42.02)(6,0)
PMechaOnPltSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnPltSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet=(-131.85,+556.66,+307.59,-179.98,-0.29,-179.49)(7,0)
PMechaOnRoboGet_1=(-80.00,+556.66,+307.20,-179.97,-0.29,-179.49)(7,0)
PMechaOnRoboGet_2=(-80.00,+316.96,+419.50,+180.00,+0.00,+180.00)(7,0)
PMechaOnRoboGet_3=(+0.00,+350.00,+480.00,-180.00,+0.00,-180.00)(7,0)
PMechaOnRoboGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet=(-131.65,+556.36,+307.20,-179.98,-0.29,-179.49)(7,0)
PMechaOnRoboSet_1=(-80.00,+556.36,+307.20,-179.98,-0.29,-179.49)(7,0)
PMechaOnRoboSet_2=(+40.77,+316.96,+419.50,+180.00,+0.00,+180.00)(7,0)
PMechaOnRoboSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaOnRoboSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead=(+600.00,-152.00,+403.00,-180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+600.00,-152.00,+450.00,-180.00,+0.00,+90.00)(7,0)
PTicketRead_2=(+198.38,+259.83,+450.00,+180.00,+0.00,+128.47)(7,0)
PTicketRead_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
JActive=(+1.36,-5.10,+116.17,+0.00,+68.93,+1.35)
Jmove=(+1.36,-15.80,+124.16,+0.00,+71.59,+1.35,+0.00,+0.00)
JTaihi=(+0.00,-15.80,+124.16,+0.00,+71.59,+0.00,+0.00,+0.00)
