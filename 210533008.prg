1 ' ===================================
2 '
3 '  21053001 STEP5 Assy5�v���O����
4 '
5 ' �쐬�ҁFM.Hayakawa
6 ' �쐬���F2021.07.09
7 ' Ver 0.1 2021.07.09 STEP1���痬�p
8 ' ===================================
9 '===== <Insight�萔> =====
10 '===== <Insight�ϐ���`> =====
11 Dim PInspPosition(30)               '�摜����Function���n���p�ʒu�ϐ�
12 Dim MInspGroup%(30)                 '�摜����Function���n���p�ϐ�
13 Def Inte MIN_IS_Ready               '�y����IO�zInsight����OK
14 Def Inte MIN_IS_JobLoadOK           '�y����IO�zInsight�W���u���[�h����I��
15 Def Inte MIN_IS_JobLoadNG           '�y����IO�zInsight�W���u���[�h�ُ�I��
16 Def Inte MIN_IS_InspGSetOK          '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
17 Def Inte MIN_IS_InspGSetNG          '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
18 Def Inte MIN_IS_InspOK              '�y����IO�zInsight����OK
19 Def Inte MIN_IS_InspNG              '�y����IO�zInsight����NG
20 Def Inte MIN_IS_InspErr             '�y����IO�zInsight�����ُ�I��
21 Def Inte MIN_IS_InspCapDone         '�y����IO�zInsight�����摜�捞����
22 '
23 Def Inte MIN_IS_ErrNum              '�y����IO�zInsight�����G���[�ԍ��擾�J�n�A�h���X(16bit)
24 'Output Signal
25 Def Inte MOUT_IS_JobLoadReq         '�y�o��IO�zInsight JOB���[�h�v��
26 Def Inte MOUT_IS_InspGSetReq        '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
27 Def Inte MOUT_IS_Insp               '�y�o��IO�zInsight �������s�v��
28 '
29 Def Inte MOUT_IS_JobNum             '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
30 Def Inte MOUT_IS_InspGNum           '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
31 '
32 Def Inte MOUT_InspErrNum            '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
33 Def Inte MOUT_InspNGStepNum         '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
34 '��Ɨp�ϐ�
35 Def Inte MInspErrNum                '�������s�G���[�ԍ�
36 Def Inte MInspNGStepNum             '�������sNGStep�ԍ�
37 Def Inte MRtn                       'Function�߂�l�擾�p
38 Def Inte MRtn2                      'Function�߂�l�擾�p
39 Def Inte MRet3                      'Function�߂�l�擾�p
40 Def Inte MGRtn                      'Function�߂�l�擾�p �l�W�����@
41 Def Inte MInspErrNumSub             '�������s�G���[�ԍ�sub�@20190820�ǉ�
42 Def Inte MovrdA                     '�l�W����Ovrd �ϗp
43 Def Float MSpdA                     '�l�W����Spd�@�ϗp
44 Def Pos PTemp                       '�l�W���ߏ��ʒu�v�Z�p
45 '===== <Insight�ϐ��ݒ�> =====
46 MIN_IS_Ready%        =   11380      '�y����IO�zInsight����OK
47 MIN_IS_JobLoadOK%    =   11381      '�y����IO�zInsight�W���u���[�h����I��
48 MIN_IS_JobLoadNG%    =   11382      '�y����IO�zInsight�W���u���[�h�ُ�I��
49 MIN_IS_InspGSetOK%   =   11383      '�y����IO�zInsight�����O���[�v�ԍ��ݒ萳��I��
50 MIN_IS_InspGSetNG%   =   11384      '�y����IO�zInsight�����O���[�v�ԍ��ݒ�ُ�I��
51 MIN_IS_InspOK%       =   11385      '�y����IO�zInsight����OK
52 MIN_IS_InspNG%       =   11386      '�y����IO�zInsight����NG
53 MIN_IS_InspErr%      =   11387      '�y����IO�zInsight�����ُ�I��
54 MIN_IS_InspCapDone%  =   11388      '�y����IO�zInsight�����摜�捞����
55 MIN_IS_ErrNum%       =   11408      '�y����IO�zInsight�����G���[�ԍ��J�n�A�h���X(16bit)
56 'Output Signal
57 MOUT_IS_JobLoadReq%  =   12370      '�y�o��IO�zInsight JOB���[�h�v��
58 MOUT_IS_InspGSetReq% =   12371      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�v��
59 MOUT_IS_Insp%        =   12372      '�y�o��IO�zInsight �������s�v��
60 MOUT_IS_JobNum%      =   12384      '�y�o��IO�zInsight JOB�ԍ��ݒ�J�n�A�h���X(16bit)
61 MOUT_IS_InspGNum%    =   12400      '�y�o��IO�zInsight �����O���[�v�ԍ��ݒ�J�n�A�h���X(16bit)
62 MOUT_InspErrNum%     =   12416      '�y�o��IO�z�������s�G���[�ԍ��J�n�A�h���X(16bit)
63 MOUT_InspNGStepNum%  =   12432      '�y�o��IO�z�������sNGStep�ԍ��J�n�A�h���X(16bit)
64 '===== <�d�h���ϐ���`> =====
65 X20_Driver=11248                    '�d�h���X�e�C�^�X1�@Driver Status 1
66 X21_Driver=11249 '�d�h���X�e�C�^�X2  Driver Status 2
67 X22_Driver=11250 '�d�h���X�e�C�^�X3  Driver Status 3
68 X23_Driver=11251 '�d�h���X�e�C�^�X4  Driver Status 4
69 X24_Driver=11252 '�d�h���G���[���b�Z�[�W1 Driver Error E1
70 X25_Driver=11253 '�d�h���G���[���b�Z�[�W2 Driver Error E2
71 X26_Driver=11254 '�d�h���G���[���b�Z�[�W3 Driver Error E3
72 X27_Driver=11255 '�d�h���G���[���b�Z�[�W4 Driver Error E4
73 X28_Driver=11256 '�d�h���g�[�^���G���[�V�O�i�� Total Error
74 X29_Driver=11257 '�d�h���I���V�O�i�� Comlete signal
75 X2A_Driver=11258 '�d�h���G���[���b�Z�[�W5 Driver Error E5
76 '11584   'toRB�g���N�h���C�o-COMP_ERR���M
77 Y60_Driver=12240 '�d�h�������v��� CCW
78 Y61_Driver=12241 '�d�h�����v��� CW
79 Y62_Driver=12242 '�o���N�Z�b�e�B���O BANK C1
80 Y63_Driver=12243 '�o���N�Z�b�e�B���O BANK C2
81 Y64_Driver=12244 '�o���N�Z�b�e�B���O BANK C3
82 Y65_Driver=12245 '�v���O�����Z�b�e�B���O PRG SET F1
83 Y66_Driver=12246 '�v���O�����Z�b�e�B���O PRG SET F2
84 Y67_Driver=12247 '�v���O�����Z�b�e�B���O PRG SET F3
85 X34_ScrewReady1=11259 '�˂�����1�@Read
86 '===== <�d�h���萔> =====
87 Dim PScrewPos(10)       '�l�W���ߗpFunction�����ϐ�
88 Dim PGetScrewPos(10)    '�˂������@����˂��𓾂�Function�����ϐ�
89 Dim PEscapePosi(10)
90 MLoopCnt% = 0'
91 '===== <���{�b�g�萔> =====
92 '===== <���{�b�g�ϐ���`> =====
93 MRBTOpeGroupNo = 0      '���{�b�g����ԍ�������
94 MCommentD1001 = 0
95 MCommentD1002 = 0
96 MCommentD1003 = 0
97 MScreenNo = 0
98 '
99 MCommentTSU = 0
100 MCommentTSD = 0
101 '�E�B���h��ʔԍ��ݒ�
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
114 MClear% = 0        'KEY_�̃N���A
115 MAbout% = 1        'KEY_��~
116 MNext% = 2         'KEY_���̃X�e�b�v�ֈڍs
117 MContinue% = 3     'KEY_�p�� �ēx����������s��
118 '
119 Def Inte MNgProcess
120 MNgProcess% = 5      'KEY_NG
121 '
122 MAssyOK% = 6       '�g������
123 MPass% = 7         '�H���p�X
124 MPiasNG% = 8       'Pias�m�F������NG
125 '
126 '�������pKEY�ԍ�   '
127 MRobotInit1% = 11  '�����ʒu�p
128 MRobotInit2% = 12  '�����ʒu�p
129 MRobotInit3% = 13  '�����ʒu�p
130 MRobotInit4% = 14  '�����ʒu�p
131 '
132 MIN_INIT1REQUEST% = 11568 'toRBT_���{�b�g�����ʒu1�v��
133 MIN_INIT2REQUEST% = 11569 'toRBT_���{�b�g�����ʒu2�v��
134 MIN_INIT3REQUEST% = 11570 'toRBT_���{�b�g�����ʒu3�v��
135 MIN_INIT4REQUEST% = 11571 'toRBT_���{�b�g�����ʒu4�v��
136 '
137 MOUT_INIT1RECIVE% = 12560 'toPLC_���{�b�g�����ʒu1��M
138 MOUT_INIT2RECIVE% = 12561 'toPLC_���{�b�g�����ʒu2��M
139 MOUT_INIT3RECIVE% = 12562 'toPLC_���{�b�g�����ʒu3��M
140 MOUT_INIT4RECIVE% = 12563 'toPLC_���{�b�g�����ʒu4��M
141 '
142 MOK% = 1               '�e����p
143 MNG% = 0               '�e����p
144 MTIMEOUT% = -1         '�e����p
145 MJudge% = 0            '������i�[�p
146 '
147 MRECIVETIME& = 0
148 MSETTIMEOUT10& = 10000&                '10�b�ݒ�
149 MSETTIMEOUT03& = 3000&                 '3�b�ݒ�
150 MSETTIMEOUT01& = 1000&                 '1�b�ݒ�
151 MSETTIMEOUT05& = 5000&                 '5�b�ݒ�
152 MSETTIMEOUT009& = 900&                 '0.9�b�ݒ�
153 MSETTIMEOUT008& = 800&                 '0.8�b�ݒ�
154 MSETTIMEOUT007& = 700&                 '0.7�b�ݒ�
155 MSETTIMEOUT006& = 600&                 '0.6�b�ݒ�
156 MSETTIMEOUT005& = 500&                 '0.5�b�ݒ�
157 MSETTIMEOUT004& = 400&                 '0.4�b�ݒ�
158 MSETTIMEOUT003& = 300&                 '0.3�b�ݒ�
159 MIN_PIAS_Use% = 11363                  'PIAS FLG ON
160 MIN_PIAS_ComOK% = 11552                'PC�ʐMOK
161 MIN_PIAS_ComTimeOut% = 11576           'PC�ʐM�m�F�^�C���A�E�g
162 MIN_PIAS_ComNG% = 11553                'PC�ʐMNG
163 MOUT_PIAS_ComCheck% = 12544            'PC�ʐM�m�F�v��
164 MOUT_PIAS_Missing_Process% = 12546     '�H�������m�F�v��
165 MIN_PIAS_ModelTypeNG% = 11554          '���f���d��NG
166 MIN_PIAS_ProcessHistryNG% = 11555      '�O�H������NG
167 MIN_PIAS_ProcessHistryOK% = 11556      '�O�H������OK
168 MIN_PIAS_ProcessHistryErr% = 11557     '�H�����������G���[
169 MIN_PIAS_MyProcessComp% = 11573        '���H����������
170 MIN_PIAS_ProcessHistryTimeOut% = 11578 '�H�������^�C���A�E�g
171 MOUT_OKNG% = 12549                     'PLC OUT ��OK=1, NG=0 �o��
172 '
173 MOUT_PiasPCBNumberCheck = 12557        '��ԍ��ƍ�
174 MIN_PiasPCBNumberOK% = 11566          '��ԍ�OK
175 MIN_PiasPCBNumberNG% = 11565          '��ԍ�NG
176 MIN_PiasPCBNumberErr% = 11567         '��ԍ������G���[
177 '
178 MOUT_PiasAssyResultOK% = 12549    '�g��OK
179 MOUT_PiasAssyResultNG% = 12550    '�g��NG
180 MOUT_PiasAssyResultWr% = 12548    '�H��������������
181 '
182 MIN_PiasProcessNG% = 11559        '�H����������NG
183 MIN_PiasProcessOtherErr% = 11560  '�H�����������G���[(�Ȃ񂩂̃g���u��)
184 MIN_PiasProcessOK% = 11558        '�H����������OK
185 '
186 MIN_Insight_Use% = 11369               '�摜�m�FON
187 MIN_TorqueCheck% = 11348               '�g���N�`�F�b�N
188 '
189 MOUT_PATLIGHT_ON% = 12354          'PATLIGHT���쌠
190 MOUT_RED_LIGHT% = 12356            'PATLIGHT �� �_��
191 MOUT_RED_FLASH% = 12357            'PATLIGHT �� �_��
192 MOUT_YELLOW_LIGHT% = 12358         'PATLIGHT �� �_��
193 MOUT_YELLOW_FLASH% = 12359         'PATLIGHT �� �_��
194 MOUT_GREEN_LIGHT% = 12360          'PATLIGHT �� �_��
195 MOUT_GREEN_FLASH% = 12361          'PATLIGHT �� �_��
196 '
197 MOUT_ST_DATETIME% = 12551          '�g���J�n���t����
198 MOUT_ED_DATETIME% = 12552          '�g���I�����t����
199 '
200 MOUT_TORQUE_CHECK% = 12367         'PLC�փg���N�`�F�b�N���𑗐M
201 '
202 MIN_ASSY_CANCEL% = 11366           '�g�����s�����̃t���O
203 '
204 MLoopFlg% = 0                      'KEY���͌��OK or NG���e
205 MopeNo% = 0
206 MRtn% = 0
207 MRet = 0
208 MRet3% = 0
209 '
210 Def Inte MInputQty          '������ ���Z�ϐ�
211 Def Inte MAssyOkQty         '�g���n�j�� ���Z�ϐ�
212 Def Inte MAssyNgQty         '�g���m�f�� ���Z�ϐ�(���g�p)
213 Def Inte MSuctionErrQty     '�z���G���[�� 2022/04/27 �n��
214 Def Inte MScrewNo
215 Def Inte MReTry
216 '===== <IO�ϐ���`> =====
217 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
218 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
219 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
220 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
221 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
222 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
223 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
224 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
225 '
226 Def Inte Y6A_VV1            ' �A�[����[�@�l�W�z���o���u
227 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
228 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
229 '
230 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
231 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
232 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
233 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
234 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
235 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
236 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
237 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
238 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
239 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
240 '
241 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
242 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
243 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
244 '
245 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
246 '
247 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
248 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
249 '
250 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
251 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
252 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
253 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
254 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
255 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
256 '
257 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
258 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
259 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
260 '
261 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
262 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
263 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
264 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
265 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
266 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
267 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
268 Y6A_VV1%    =  12250    ' �A�[����[�@�l�W�z���o���u
269 Y6B_VB1%    =  12251    '�A�[����[�@�z���j��o���u
270 MOUT_VB1%   =  12251    ' �A�[����[�@�l�W�z���j��o���u
271 '
272 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
273 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
274 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
275 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
276 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
277 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
278 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
279 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
280 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
281 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
282 '
283 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
284 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
285 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
286 '
287 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
288 '
289 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
290 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
291 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
292 '
293 '����
294 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
295 Def Inte MOn                            '�o��=1
296 Def Inte MOff                           '�o��=0
297 '
298 '�˂����ߑ��u_�o�̓A�h���X
299 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
300 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
301 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
302 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
303 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
304 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
305 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
306 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
307 '�˂����ߑ��u_���̓A�h���X
308 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
309 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
310 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
311 Def Inte MIN_ScwT_Case1                 '����1��~����M
312 Def Inte MIN_ScwT_Case2                 '����2��~����M
313 Def Inte MIN_ScwT_Case3                 '����3��~����M
314 Def Inte MIN_ScwT_Case4                 '����4��~����M
315 Def Inte MIN_ScwT_Case5                 '����5��~����M
316 '
317 Dim MScwT_Case1%(2)               '����1��~�ϐ�
318 Dim MScwT_Case2%(2)               '����2��~�ϐ�
319 Dim MScwT_Case3%(2)               '����3��~�ϐ�
320 Dim MScwT_Case4%(2)               '����4��~�ϐ�
321 Dim MScwT_Case5%(2)               '����5��~�ϐ�
322 '
323 '����
324 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
325 MOn% = 1                                 '�o�� = 1
326 MOff% = 0                                '�o�� = 0
327 '
328 '�˂����ߋ@_�A�h���X�ݒ�
329 MOUT_ScwT_ComChk% = 12832               '�ʐM�m�F���M
330 MOUT_ScwT_ST% = 12865                   '�˂����ߊJ�n�𑗐M
331 MOUT_ScwT_ReSTOK% = 12866               '�ĊJ�n��M�𑗐M
332 MOUT_ScwT_FinOK% = 12868                '�˂����ߊ�����M�𑗐M
333 MOUT_ScwT_Case1OK% = 12874              '����1��~��M�𑗐M
334 MOUT_ScwT_Case2OK% = 12875              '����2��~��M�𑗐M
335 MOUT_ScwT_Case3OK% = 12876              '����3��~��M�𑗐M
336 MOUT_ScwT_Case4OK% = 12877              '����4��~��M�𑗐M
337 MOUT_ScwT_Case5OK% = 12878              '����5��~��M�𑗐M
338 '
339 MIN_ScwT_comOK% = 11840                 '�˂����ߑ��u����ԐM
340 MIN_ScwT_STRec% = 11873                 '�˂����ߊJ�n����M
341 MIN_ScwT_ReST% = 11874                  '�ĊJ�n����M
342 MIN_ScwT_Fin% = 11876                   '�˂����ߊ�������M
343 MIN_ScwT_Case1% = 11882                 '����1��~�ҋ@����M
344 MIN_ScwT_Case2% = 11883                 '����2��~�ҋ@����M
345 MIN_ScwT_Case3% = 11884                 '����3��~�ҋ@����M
346 MIN_ScwT_Case4% = 11885                 '����4��~�ҋ@����M
347 MIN_ScwT_Case5% = 11886                 '����5��~�ҋ@����M
348 '
349 MScwT_Case1%(1) = MIN_ScwT_Case1%
350 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
351 MScwT_Case2%(1) = MIN_ScwT_Case2%
352 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
353 MScwT_Case3%(1) = MIN_ScwT_Case3%
354 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
355 MScwT_Case4%(1) = MIN_ScwT_Case4%
356 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
357 MScwT_Case5%(1) = MIN_ScwT_Case5%
358 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
359 '
360 '�ݒ� InitialZoneB�Ŏg�p����ϐ�
361 Def Pos PActive       '�������W�n �ʒu�ϐ� ���݈ʒu
362 Def Pos Pmove         '�������W�n �ʒu�ϐ� �ړ���
363 Def Jnt JActive       '�֐ߍ��W�n �ʒu�ϐ� ���݈ʒu
364 Def Jnt Jmove         '�֐ߍ��W�n �ʒu�ϐ� �ړ���
365 Def Jnt JTaihi        '�֐ߍ��W�n �ʒu�ϐ� �ޔ��|�W�V���� �e�B�[�`���O�Őݒ�
366 Def Inte MRecoveryPass      '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s
367 Def Inte MJ6          'J6���̒l���r����ׂ̕ϐ�
368 Def Inte MStandby              '�ҋ@�ʒu�m�F�t���O
369 Def Inte MRecoveryChuckOpen    '�`���b�N����t���O�i���A����O�j���͂ݑ΍�
370 '�����Ӂ������ʒu��ύX�������ɂ́A�ύX���K�v�I
371 '
372 '
373 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
374 Function M% fnAssyStart
375     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
376 '
377 '   BaseUnit6�ʐM�m�F
378     *RE_COM_CHECK
379     M_20# = MClear%
380     MRtn = 1    '������
381     If M_In(11920) = 0 Then     'BaseUnit6���E�����̃t���O�𗧂ĂĂ��Ȃ�
382         If M_In(11930) = 0 And M_In(11931) = 0 Then   '�ʐM�ɂĉ�]�p�s��
383             Dly 2.3                                   '��]�҂�
384             If M_In(11930) = 0 And M_In(11931) = 0 Then  '������x�m�F
385                 MRtn = 0                              '�ʐM�ɂĈُ�
386                 Break
387             EndIf
388             Break
389         EndIf
390         Break
391     EndIf
392     If MRtn = 1 Then GoTo *BU6Com_OK    '�ʐMOK�Ȃ烉�x���W�����v
393     fErrorProcess(11,298,284,0)           '0,284��298,287�ɕύX6/2����
394     If M_20# = MNext% Then GoTo *RE_COM_CHECK
395     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
396     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
397     If M_20# = MContinue% Then GoTo *RE_COM_CHECK
398     *BU6Com_OK
399 '
400 '
401 ' PIAS�`�P�b�g�Ǎ��ݍH�������m�F
402     M_20# = MClear%                       '������
403 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
404 '        MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
405 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
406 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
407 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' ���j���[�֖߂�
408 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NG���H�������ɏ����ݎ��̍H����
409 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NG���H�������ɏ����ݎ��̍H����
410 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' ����NG, �H������
411 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK�ȊO�͑g���I��
412 '    EndIf
413 '    '
414 '    '���W�ړ�
415 '    '
416 '    '����xx��~
417 '    fScewTCaseStop(MScwT_Case5%)
418 '    '
419 '    '�x�[�X���j�b�gKEY
420 '    Wait M_In(MTEST_KEY%) = MOn%
421 '    '
422 '    '�ĊJ�n
423 '    fScewTReStart()
424 '    '
425 '    '���W�ړ�
426 '    '
427 '    '�˂����ߊ���
428 '    Mret% = fScewTFinish()
429 ' �l�W���߃e�X�g�I��
430 ' PIAS�e�X�g -----------
431 '    MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
432 '    MRet% = fnPiasWrite(MNG%)
433  '   MRet% = fnPCBNumberCheck()
434 ' PIAS�e�X�g�I�� -------
435 '
436     '�g���J�n(9/6�ǉ�(����))
437     '�v���O�������_
438     Ovrd 100
439     ' �n���h��ԏ�����(10/29�ǉ�M.H)(2/11�C��(����))
440     Cmp Off                     '�R���v���C�A���X���[�h�I��
441     ColChk On                   '�Փˌ��mON
442     If M_In(11266) Then
443         M_Out(12256) = 0
444         M_Out(12257) = 1
445     EndIf
446     If M_In(11269) Then
447         M_Out(12258) = 0
448         M_Out(12259) = 1
449     EndIf
450     If M_In(11271) Then
451         M_Out(12260) = 0
452         M_Out(12261) = 1
453     EndIf
454     *WAIT_HAND_INI
455     If M_In(11265) = 1 And M_In(11268) = 1 And M_In(11270) = 1 Then GoTo *CompHandIni Else GoTo *WAIT_HAND_INI
456     *CompHandIni
457     M_Out(12257) = 0
458     M_Out(12259) = 0
459     M_Out(12261) = 0
460 '
461 '
462 'Dly 5                                  '�f�o�b�O�p(22/09/30����)
463     ' �˂����ߋ@�e�X�g�p ----------
464      Mret% = fScrewTcomChk()
465     If Mret% = -1 Then GoTo *ASSY_ERROR_END
466     ' �˂����ߋ@�ʐM�J�n
467 '    fScrewTStart()           '�����ʒu�ύX2/27����
468     '�`�P�b�gID��ǂ�
469     M_Out(12262) = 1 Dly 0.5 '�{�̈ʒu���ߏo�[ON
470     PTemp = P_Curr
471     MRtn = 0
472 '    If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
473 '        If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
474 '            If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
475 '                MRtn = 1
476 '            EndIf
477 '        EndIf
478 '    EndIf
479 '    If MRtn = 1 Then
480 '        Mov PTicketRead
481 '    Else
482 '        Cnt 1 , 10 , 10
483 '        Mov PInitialPosition
484 '        Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
485 '        Cnt 0
486 '        Mvs PTicketRead             'ID�ǂ݈ʒu
487 '    EndIf
488 '
489 ' 2022/04/12 ���S�����֏����ύX �n��
490 ' PInitialPosition �ݐ� MStandby=2
491 ' PTicketRead_1 �ݐ� MStandby=1
492 '
493     MStandby = 0    '�ҋ@�ʒu�t���O��������
494     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
495         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
496             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
497                 MStandby = 2
498             EndIf
499         EndIf
500     EndIf
501     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
502         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
503             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
504                 MStandby = 1
505             EndIf
506         EndIf
507     EndIf
508     If MStandby = 2 Then
509         Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
510         Cnt 0
511     EndIf
512     If MStandby <> 0 Then GoTo *PositionOK
513     fErrorProcess(11,230,281,0)            '�����ʒu�ɂ��Ȃ����̓G���[�ɂ���
514     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
515     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
516     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
517     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
518     *PositionOK
519 '
520     Mvs PTicketRead             'ID�ǂ݈ʒu
521 '
522     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
523     M_Out(12258) = 1            'DVD���J�`���b�N��ON
524 '
525     '
526     MRtn = 1        'MRtn������
527 *RE_TICKET_READ
528 '    MRtn = fnPiasCheck()               'ID�ǂݎ��
529 'PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
530 'MInspGroup%(1) = 1              '����G�ԍ�
531 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
532 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
533     MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
534     '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
535     '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
536 EndIf
537 If MRtn = 1 Then GoTo *CompRead
538 '
539     '�G���[�����i�ʒu���߂�����
540 *RE_ERR_REL_1
541 If M_20# = MContinue% Then M_20# = MRtn
542 M_Out(12263) = 1 Dly 0.5    '���i�ʒu���߂�����
543 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)
544 '
545 If MRtn = 1 Then GoTo *CompErrorRelease
546 MRtn = M_20#        'M_20#�ꎞ���
547 M_20# = MClear%
548 fErrorProcess(11,234,284,0)     '�ʒu���ߖߒ[�G���[
549 If M_20# = MContinue% Then GoTo *RE_ERR_REL_1
550 If M_20# = MNext% Then M_20# = MRtn
551 If M_20# = MNgProcess% Then M_20# = MAbout%
552 *CompErrorRelease
553 '
554 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
555 If M_20# = MNext% Then M_20# = MPass%
556 Mvs PTicketRead_1                         '22/04/07 �ǉ� �n��
557 GoTo *ASSY_ERROR_END
558 *CompRead
559     fScrewTStart()           '�����ʒu�ύX2/27����)
560 '
561 '
562 ''    Wait M_In(11888) = 1        '�˂����{2��~1��M(�����ʒu�ύX2/7����)
563 '    MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
564 '    *RE_ERR_REL_2
565 '    If M_20# = MContinue% Then M_20# = MRtn2
566 '    If MRtn = 0 Then
567 '        MRtn2 = 1       'MRtn2������
568 '        M_Out(12263) = 1 Dly 0.5    '���i�ʒu���߂�����
569 '        Mov PInitialPosition  '"�C�j�V�����ɖ߂铮��"
570 '        MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
571 '        If MRtn2 = 0 Then
572 '            MRtn2 = M_20#                   '�ʒu���ߖߒ[�G���[�Ȃ�M_20#�������ꎞ���
573 '            M_20# = MClear%                 'M_20#������
574 '            fErrorProcess(11,234,284,0)     '�ʒu���ߖߒ[�G���[
575 '            If M_20# = MNext% Then M_20# = MRtn2        'M_20#�ɔ����l����
576 '                '�ʒu���߃G���[���������čH���𔲂���ꍇ��~�������s��
577 '            If M_20# = MNgProcess% Then M_20# = MAbout%
578 '            Break
579 '        EndIf
580 '        Break
581 '            EndIf
582 '    If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
583 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
584     '
585     '�p���b�g���琻�i�����
586     '
587     *RE_POSITIONING
588     '
589     M_Out(12262) = 1 Dly 0.5 '�{�̈ʒu���ߏo�[ON
590 '    Wait M_In(11273) = 1     '�{�̈ʒu���ߏo�[���o
591     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '�{�̈ʒu���ߏo�[���o
592     If MRtn = 1 Then GoTo *CompPositioning
593     fErrorProcess(11,231,282,0)
594     If M_20# = MNext% Then M_20# = MClear%
595     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
596     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
597     If M_20# = MContinue% Then GoTo *RE_POSITIONING
598     *CompPositioning
599 '
600     Mov PProductOnPltGet_2      '�{�̎󂯎������_
601 '
602 ''    Wait M_In(11888) = 1        '�˂����{2��~1��M(�����ʒu�ύX3/14����)
603 '    MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
604 '    *RE_ERR_REL_2
605 '    If M_20# = MContinue% Then M_20# = MRtn2
606 '    If MRtn = 0 Then
607 '        MRtn2 = 1       'MRtn2������
608 '        M_Out(12263) = 1 Dly 0.5    '���i�ʒu���߂�����
609 '        Mov PInitialPosition  '"�C�j�V�����ɖ߂铮��"
610 '        MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
611 '        If MRtn2 = 0 Then
612 '            MRtn2 = M_20#                   '�ʒu���ߖߒ[�G���[�Ȃ�M_20#�������ꎞ���
613 '            M_20# = MClear%                 'M_20#������
614 '            fErrorProcess(11,234,284,0)     '�ʒu���ߖߒ[�G���[
615 '            If M_20# = MNext% Then M_20# = MRtn2        'M_20#�ɔ����l����
616 '                '�ʒu���߃G���[���������čH���𔲂���ꍇ��~�������s��
617 '            If M_20# = MNgProcess% Then M_20# = MAbout%
618 '            Break
619 '        EndIf
620 '        Break
621 '            EndIf
622 '    If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
623 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
624 '
625 '    Mov PProductOnPltGet_1      '�{�̎󂯎����
626     '
627     *RE_PLT_GET_1
628     '
629     M_Out(12256) = 0            '�{�̃`���b�N��OFF
630     M_Out(12257) = 1            '�{�̃`���b�N�JON
631     '
632 '    Wait M_In(11265) = 1        '�{�̃`���b�N�J���o
633     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
634     If MRtn = 1 Then GoTo *CompPltGet1
635     fErrorProcess(11,244,284,0)
636     If M_20# = MNext% Then M_20# = MClear%
637     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
638     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
639     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
640     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
641     *CompPltGet1
642     '
643     Mov PProductOnPltGet_1      '�{�̎󂯎����
644     '
645     Ovrd 25
646 '    Fine 0.05 , P
647     Mvs PProductOnPltGet        '�{�̎󂯎��ʒu
648     Dly 0.1
649     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
650     M_Out(12256) = 1            '�{�̃`���b�N��ON
651 '    Fine 0 , P
652     '
653     M_Out(12263) = 1 Dly 0.5                    '�{�̈ʒu���ߖߒ[ON
654 '    Wait M_In(11274) = 1     '�{�̈ʒu���ߖߒ[���o
655     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)    '�{�̈ʒu���ߖߒ[���o
656     If MRtn = 1 Then GoTo *CompPltGet2
657     M_Out(12256) = 0                            '�{�̃`���b�N��OFF
658     M_Out(12257) = 1                            '�{�̃`���b�N�JON
659     Dly 2.0
660     Mvs PProductOnPltGet_1
661     Mov PProductOnPltGet_2
662     fErrorProcess(11,234,284,0)
663     If M_20# = MNext% Then M_20# = MClear%
664     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
665     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
666     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
667     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
668     Mov PProductOnPltGet_1
669     Mvs PProductOnPltGet
670     M_Out(12257) = 0                            '�{�̃`���b�N�JOFF
671     M_Out(12256) = 1                            '�{�̃`���b�N��ON
672     Dly 2.0
673     *CompPltGet2
674     '
675 '    Wait M_In(11264) = 1        '�{�̌��o
676     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '�{�̌��o
677     If MRtn = 1 Then GoTo *CompPltGet3
678     M_Out(12256) = 0            '�{�̃`���b�N��OFF
679     M_Out(12257) = 1            '�{�̃`���b�N�JON
680     Dly 2.0
681     Mvs PProductOnPltGet_1
682     Mov PProductOnPltGet_2
683     fErrorProcess(11,252,284,0)
684     If M_20# = MNext% Then M_20# = MClear%
685     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
686     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
687     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
688     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
689     Mov PProductOnPltGet_1
690     Mvs PProductOnPltGet
691     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
692     M_Out(12256) = 1            '�{�̃`���b�N��ON
693     Dly 2.0
694     *CompPltGet3
695     '
696 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
697     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
698     If MRtn = 1 Then GoTo *CompPltGet4
699     M_Out(12256) = 0            '�{�̃`���b�N��OFF
700     M_Out(12257) = 1            '�{�̃`���b�N�JON
701     Dly 2.0
702     Mvs PProductOnPltGet_1
703     Mov PProductOnPltGet_2
704     Dly 0.1
705     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
706     M_Out(12256) = 1            '�{�̃`���b�N��ON
707     Dly 3.0
708     fErrorProcess(11,245,284,0)
709     If M_20# = MNext% Then M_20# = MClear%
710     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
711     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
712     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
713     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
714     M_Out(12256) = 0            '�{�̃`���b�N��OFF
715     M_Out(12257) = 1            '�{�̃`���b�N�JON
716     Dly 2.0
717     Mov PProductOnPltGet_1
718     Mvs PProductOnPltGet
719     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
720     M_Out(12256) = 1            '�{�̃`���b�N��ON
721     Dly 2.0
722     *CompPltGet4
723     '
724     Dly 0.2                     '�O�̂��߃f�B���C
725     Cnt 1 , 10 , 10
726     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
727     Mvs PProductOnPltGet_1      '�{�̎󂯎����
728     MRtn = FnCtlValue2(99)       '�Ǐ��J�n�M��OFF  2022/04/28 �n��
729     Ovrd 100
730     Mov PProductOnPltGet_2      '�{�̎󂯎������_
731     '
732     '���i���˂����{2�ɒu��
733     Mov PProductOnRoboSet_3     '�o�H
734     Cnt 0
735 '    Wait M_In(11888) = 1        '�˂����{2��~1��M(�����ʒu,�����ύX3/1����)
736     MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
737     *RE_ERR_REL_2
738     If MRtn = 0 Then
739         Cnt 0
740         Mov PProductOnPltSet_2
741         Mov PProductOnPltSet_1
742         Mvs PProductOnPltSet
743         M_Out(12256) = 0        '�{�̃`���b�N��OFF
744         M_Out(12257) = 1        '�{�̃`���b�N�JON
745         Dly 2.0
746         Mvs PProductOnPltSet_1
747         Mvs PProductOnPltSet_2
748         Mov PInitialPosition
749     EndIf
750     If MRtn = 0 Then GoTo *ASSY_ERROR_END
751     '
752     *RE_ROBO_SET_1
753     '
754     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
755     M_Out(12258) = 1            'DVD���J�`���b�N��ON
756 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
757     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
758     If MRtn = 1 Then GoTo *CompRoboSet1
759     fErrorProcess(11,269,284,0)
760     If M_20# = MNext% Then M_20# = MClear%
761     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
762     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
763     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
764     *CompRoboSet1
765 '
766     Mvs PProductOnRoboSet_2     '�˂����{���i�u�������_
767 '    Mvs PProductOnRoboSet_4     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)
768     Ovrd 25
769     Mvs PProductOnRoboSet_1     '�˂����{���i�u�����
770     Mvs PProductOnRoboSet       '�˂����{���i�u���ʒu
771     M_Out(12866) = 1 Dly 0.3    '�˂����{2����ĊJ(��~1�`��~2)
772 '    Wait M_In(11889) = 1        '�˂����{2��~2��M
773     MScrewRoboNgFlg% = 0
774     MRtn = fScrewTighenRoboCheck(11889)    '��~��Ԃ���M����
775     If MRtn = 0 Then
776         MScrewRoboNgFlg% = 1
777     EndIf
778 '
779     *RE_ROBO_SET_2
780 '
781     M_Out(12256) = 0            '�{�̃`���b�N��OFF
782     M_Out(12257) = 1            '�{�̃`���b�N�JON
783 '    Wait M_In(11265)            '�{�̃`���b�N�J���o
784     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
785     If MRtn = 1 Then GoTo *CompRoboSet2
786     fErrorProcess(11,244,284,0)
787     If M_20# = MNext% Then M_20# = MClear%
788     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
789     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
790     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_2
791     *CompRoboSet2
792     '
793     Mvs PProductOnRoboSet_1     '�˂����{���i�u�����
794     Mvs PProductOnRoboSet_2     '�˂����{���i�u�������_
795 '    Mvs PProductOnRoboSet_4     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)
796     Ovrd 100
797     Cnt 1 , 10 , 10
798     Mov PProductOnRoboSet_3     '�o�H
799     '
800     If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition
801     If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
802 '
803 '
804 '
805     '
806     '�`���g�X���C�_�[������
807     Cnt 1 , 10
808     Mov PPushTilt_3             '�`���g�X���C�_�[���������ς����_
809     Cnt 0
810     Mov PPushTilt_2             '�`���g�X���C�_�[�������_
811     Ovrd 30
812     Mvs PPushTilt_1             '�`���g�X���C�_�[�������
813     Spd 1000
814     Ovrd 5
815     Mvs PPushTilt               '�`���g�X���C�_�[����
816     Spd M_NSpd
817     Ovrd 30
818     Cnt 1 , 1 , 1
819     Mvs PPushTilt_1             '�`���g�X���C�_�[�������
820     Cnt 1 , 1 , 10
821     Ovrd 100
822     Mov PPushTilt_2             '�`���g�X���C�_�[�������_
823     '
824     '�w�ʔ����(�R���v���C�A���X���[�h����11/8����)
825     Mov PPlateBackGet_2         '�w�ʔ󂯎������_
826 '    Cnt 1 , 10'�b��
827     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~2�`��~3)
828 '    MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����'12/20�ʒu�̕ύX(����)
829 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
830 '    Mov PPlateBackGet_1         '�w�ʔ󂯎����'�b��
831     Cnt 0
832     '
833     *RE_PLATE_GET
834     '
835     Fine 0.05 , P               '�t�@�C������ON
836     Ovrd 25
837     Mvs PPlateBackGet           '�w�ʔ󂯎��ʒu
838 '    Dly 0.2                     '�ꎞ�R�����g�A�E�g
839     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
840     M_Out(12256) = 1            '�{�̃`���b�N��ON
841     '
842 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
843     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
844     If MRtn = 1 Then GoTo *CompPlateGet_1
845     M_Out(12256) = 0            '�{�̃`���b�N��OFF
846     M_Out(12257) = 1            '�{�̃`���b�N�JON
847     Mvs PPlateBackGet_1
848     fErrorProcess(11,245,293,0) '284��293�ɕύX6/2����
849     If M_20# = MNext% Then M_20# = MClear%
850     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
851     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
852     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
853     Mvs PPlateBackGet           '�w�ʔ󂯎��ʒu
854     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
855     M_Out(12256) = 1            '�{�̃`���b�N��ON
856     *CompPlateGet_1
857     Fine 0 , P                  '�t�@�C������OFF
858     '
859     Ovrd 5
860     Accel 25 , 100
861     Dly 0.7                     '�f�B���C���Ԓ��ߒ�(�c���͊m��)
862 '    CmpG 0.7,0.7,,,,,,       'X,Y���Q�C����0.7�ɕύX
863 '    ColChk Off                  '�Փˌ��mOFF
864 '    Cmp Pos , &B11          'X,Y���R���v���C�A���X���[�h�J�n
865     Mov PPlateBackGet_1         '�w�ʔ󂯎����
866     Cnt 1 , 10 , 10
867 '    Cmp Off                     '�R���v���C�A���X���[�h�I��
868 '    ColChk On                   '�Փˌ��mON
869     Ovrd 50
870     Mov PPlateBackGet_2         '�w�ʔ󂯎������_
871     Ovrd 100
872     Accel 100 , 100
873     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '�w�ʃp�l���`�F�b�N
874     If MRtn = 1 Then GoTo *CompPlateGet_2
875     Cnt 0
876     fErrorProcess(11,267,293,0)                     '284��293�ɕύX6/2����
877     If M_20# = MNext% Then M_20# = MClear%
878     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
879     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
880     If M_20# = MContinue% Then
881         Mov PPlateBackGet_1
882         Dly 0.3
883         M_Out(12256) = 0            '�{�̃`���b�N��OFF
884         M_Out(12257) = 1            '�{�̃`���b�N�JON
885         Dly 2.0
886     EndIf
887     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
888     *CompPlateGet_2    '
889     '�w�ʔ�u��
890 '    Wait M_In(11889) = 1        '�˂����{2��~2��M
891     ColChk Off
892     Mov PPlateBackSet_12        '�w�ʔu�����
893     Cnt 1 , 10
894 '
895     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '������x�w�ʃp�l���`�F�b�N
896     If MRtn = 1 Then GoTo *CompPlateGet_3
897     fErrorProcess(11,267,293,0)                     '284��293�ɕύX6/2����
898     If M_20# = MNext% Then M_20# = MClear%
899     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
900     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
901     If M_20# = MContinue% Then
902         Mov PPlateBackGet_2
903         Mov PPlateBackGet_1
904         M_Out(12256) = 0            '�{�̃`���b�N��OFF
905         M_Out(12257) = 1            '�{�̃`���b�N�JON
906         Dly 2.0
907     EndIf
908     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
909     *CompPlateGet_3
910 '
911 '    ' ���i�����v�����M
912     M_Out(12787) = 1
913 '
914     MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����'12/20�ʒu�̕ύX(����)
915     If MRtn = 0 Then Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
916     If MRtn = 0 Then GoTo *ASSY_ERROR_END
917 '
918 '    Mov PPlateBackSet_11        '�o�H1
919     Ovrd 50
920 '    Mov PPlateBackSet_10        '�o�H2
921 '    Mov PPlateBackSet_9         '�o�H3
922 '    Mov PPlateBackSet_8         '�o�H4
923 '    Mov PPlateBackSet_7         '�o�H5
924     Mov PPlateBackSet_6         '�o�H6
925     Cnt 0
926     Ovrd 25
927     Mvs PPlateBackSet_5         '�o�H7
928     Mvs PPlateBackSet_4         '�o�H8
929     Mov PPlateBackSet_3         '�o�H9
930     Mov PPlateBackSet_2         '�o�H10
931     Mov PPlateBackSet_1         '�o�H11
932     Mov PPlateBackSet           '�w�ʔu���ʒu
933     *RE_PLATE_SET
934     M_Out(12256) = 0            '�{�̃`���b�N��OFF
935     M_Out(12257) = 1            '�{�̃`���b�N�JON
936     '
937 '    Wait M_In(11265)            '�{�̃`���b�N�J���o
938     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
939     If MRtn = 1 Then GoTo *CompPlateSet
940     fErrorProcess(11,244,284,0)
941     If M_20# = MNext% Then M_20# = MClear%
942     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
943     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
944     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
945     *CompPlateSet
946     '
947     ColChk On
948     Mov PPlateBackSet_12        '�w�ʔu�����
949     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`��~4)
950     Ovrd 100
951     '
952 ''    ' ���i�����v�����M(�����ʒu�ύX2/27����)
953 '    M_Out(12787) = 1
954     '�˂����{���i�N�����v�Œ�҂�
955 '    Wait M_In(11891) = 1        '�˂����{2��~4��M
956 MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
957 If MRtn = 0 Then Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
958 If MRtn = 0 Then GoTo *ASSY_ERROR_END
959     '�u���ʒu�摜����
960 '    Mov PPlateBackCheck_3       '�摜�����ʒu���
961 '    Mov PPlateBackCheck_2       '�ʉߓ_
962 '    Mvs PPlateBackCheck         '�m�F�ʒu
963     '
964     'PInspPosition(2) = PPlateBackCheck
965     'MInspGroup(2) = 2
966     'MRtn = ISInspection(PInspPosition,MInspGroup,2,-1,1)
967     'If MRtn <> 1 Then
968     '   '�G���[����
969     'EndIf
970     '
971 ''    ' ���i�����v�����M
972 '    M_Out(12787) = 1    '�����ʒu�ύX
973 '    Mov PPlateBackCheck_3       '�摜�����ʒu���
974     '
975     '�˂����{�������ݑ҂�
976     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`����)
977     '
978     'DVD���J�����
979     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
980     M_Out(12259) = 1            'DVD���J�`���b�N�JON
981     '
982     Mov PMechaGet_3             '�o�H1
983     Mov PMechaGet_2             'DVD���J�󂯎������_
984 '    Wait M_In(11272)            '���i�����@Ready
985 '    MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '���i�����@Ready
986 '    If MRtn = 0 Then
987 '        fErrorProcess()         '�G���[����
988 '    EndIf
989 '
990 '    ' ���i�����v�����M(�����ʒu�ύX)
991     fnAutoScreenComment(513)    '��ԕ\��[���i�����҂�] 2022/04/26 �n��
992 '    M_Out(12787) = 1
993     '    ' ���i���������҂�(�����ύX2/27����)
994 *RE_FEEDER_READY
995 '    Wait M_In(11810) = 1
996 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '�����҂�
997 If MRtn = 1 Then GoTo *CompFeederReady
998 '   ' ���i�����v���I��
999 M_Out(12787) = 0
1000 fErrorProcess(11,289,290,0)                '284��290�ɕύX6/2����
1001 If M_20# = MNext% Then M_20# = MClear%
1002 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1003     Mov PMechaGet_2
1004     Mov PMechaGet_3
1005     Mov PMechaGet_4
1006     Mov PInitialPosition
1007 EndIf
1008 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1009 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1010     ' ���i�����v��
1011 M_Out(12787) = 1
1012 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
1013 *CompFeederReady
1014 '    ' ���i�����v���I��
1015     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1016     M_Out(12787) = 0
1017 '
1018     Mov PMechaGet_1             'DVD���J�󂯎����
1019     '
1020     *RE_MECHA_GET_1
1021     '
1022     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1023     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1024     '
1025 '    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1026     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1027     If MRtn = 1 Then GoTo *CompMechaGet1
1028     Mov PMechaGet_2
1029     Mov PMechaGet_3
1030     Mov PMechaGet_4
1031     fErrorProcess(11,270,284,0)
1032     If M_20# = MNext% Then M_20# = MClear%
1033     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1034     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1035     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1036     Mov PMechaGet_3
1037     Mov PMechaGet_2
1038     Mov PMechaGet_1
1039     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1040     *CompMechaGet1
1041     '
1042     M_Out(12261) = 0            'DVD���J�V�����_�[��OFF
1043     M_Out(12260) = 1            'DVD���J�V�����_�[�oON
1044 '    Wait M_In(11271) = 1        'DVD���J�V�����_�[�o���o
1045     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�o���o
1046     If MRtn = 1 Then GoTo *CompMechaGet2
1047     Mov PMechaGet_2
1048     Mov PMechaGet_3
1049     Mov PMechaGet_4
1050     fErrorProcess(11,271,284,0)
1051     If M_20# = MNext% Then M_20# = MClear%
1052     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1053     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1054     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1055     Mov PMechaGet_3
1056     Mov PMechaGet_2
1057     Mov PMechaGet_1
1058     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1059     *CompMechaGet2
1060     '
1061     Ovrd 25
1062     Mvs PMechaGet               'DVD���J�󂯎��ʒu
1063     Dly 0.1
1064 '
1065     MRtn = 0
1066     MRtn2 = 0
1067     *RE_MECHA_GET_2
1068     If M_20# = MContinue% Then M_20# = MClear%
1069     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1070     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1071     '
1072 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
1073     If MRtn = 1 Then Dly 1.0
1074     If MRtn = 1 Then GoTo *CompMechaGet3
1075     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1076     If M_20# = MNext% Then GoTo *CompMechaGet3
1077     If MRtn = 1 Then GoTo *CompMechaGet3
1078     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1079     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1080     Dly 2.0
1081     Mvs PMechaGet_1
1082     Mov PMechaGet_2
1083     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1084     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1085     Mov PMechaGet_3
1086     Mov PMechaGet_4
1087     fErrorProcess(11,269,284,0)
1088     If M_20# = MNext% Then
1089         M_20# = MClear%
1090         MRtn = 1
1091     EndIf
1092     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1093     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1094     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1095     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1096     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1097     Mov PMechaGet_3
1098     Mov PMechaGet_2
1099     Mov PMechaGet_1
1100     Mvs PMechaGet
1101     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1102     *CompMechaGet3
1103     M_20# = MClear%
1104     '
1105 '    Wait M_In(11267) = 1        'DVD���J���o
1106     If MRtn2 = 1 Then GoTo *CompMechaGet4
1107     MRtn2 = frInCheck(11267,1,MSETTIMEOUT05&)   'DVD���J���o
1108     If MRtn2 = 1 Then GoTo *CompMechaGet4
1109     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1110     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1111     Dly 2.0
1112     Mvs PMechaGet_1
1113     Mov PMechaGet_2
1114     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1115     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1116     Mov PMechaGet_3
1117     Mov PMechaGet_4
1118     fErrorProcess(11,273,284,0)
1119     If M_20# = MNext% Then
1120         M_20# = MClear%
1121         MRtn2 = 1
1122     EndIf
1123     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1124     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1125     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1126     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1127     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1128     Mov PMechaGet_3
1129     Mov PMechaGet_2
1130     Mov PMechaGet_1
1131     Mvs PMechaGet
1132     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1133     *CompMechaGet4
1134     M_20# = MClear%
1135     Dly 0.5
1136     '
1137     Mvs PMechaGet_1             'DVD���J�󂯎����
1138 '    *RE_MECHA_GET_3
1139     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1140     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1141 '    Wait M_In(11270) = 1        'DVD���J�V�����_�[�ߌ��o
1142     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�ߌ��o
1143 '    If MRtn = 1 Then GoTo *CompMechaGet5       '�����ʒu�ύX2/11����
1144 '    fErrorProcess(11,272,284,0)
1145 '    If M_20# = MNext% Then M_20# = MClear%
1146 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1147 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1148 '    If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1149 '    *CompMechaGet5
1150     '
1151     If MRtn = 1 Then Ovrd 100
1152     Mov PMechaGet_2             'DVD���J�󂯎������_
1153 '    ' ���i�����v���I��
1154     M_Out(12787) = 0
1155 '    ' ���i�擾�������M(�p���X)
1156     M_Out(12800) = 1 Dly 0.5
1157     Mov PMechaGet_3             '�o�H1
1158     Mov PMechaGet_4             '�o�H2
1159 '
1160     *RE_MECHA_GET_3
1161     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1162     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1163     If MRtn = 1 Then GoTo *CompMechaGet5
1164     fErrorProcess(11,272,284,0)
1165     If M_20# = MNext% Then M_20# = MClear%
1166     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1167     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1168     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1169     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1170     *CompMechaGet5
1171     '
1172     'DVD���J�����u����֒u��
1173 '    Wait M_In(11920) = 0             'BaseUnit6�����u����t���O�m�F(�����ʒu�ύX2/11����)
1174 '
1175 '   ���u���䂪��]�����m�F
1176     *Loop_CW_CCW_1
1177     If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_1 Else GoTo *Loop_CW_CCW_1
1178     *Next_CW_CCW_1
1179     If M_In(11920) = 0 Then GoTo *OK_FLG_1 Else GoTo *Loop_CW_CCW_1          'BaseUnit6�����u����t���O�m�F
1180     *OK_FLG_1
1181 '
1182     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1183     '
1184     'DVD���J�����u����ɒu����Ă��Ȃ����̊m�F(�ǉ���������10/1����)
1185     MRtn = 1
1186     If M_In(11931) = 1 Then          '��]�����̊m�F(CW����)
1187         If M_In(11928) = 0 Then      'BaseUnit5����DVD���J���u����Ă����ꍇ
1188             M_Out(12912) = 0         '���u����t���O���(��]�\��Ԃɂ��邽��)
1189             Wait M_In(11930) = 1     '���u�����]�҂�
1190             MRtn = 0
1191         EndIf
1192     ElseIf M_In(11930) = 1 Then      '��]�����̊m�F(CCW����)
1193         If M_In(11929) = 0 Then      'BaseUnit5����DVD���J���u����Ă����ꍇ
1194             M_Out(12912) = 0         '���u����t���O���(��]�\��Ԃɂ��邽��)
1195             Wait M_In(11931) = 1     '���u�����]�҂�
1196             MRtn = 0
1197         EndIf
1198     Else
1199         MRtn = 0
1200     EndIf
1201     If MRtn = 0 Then GoTo *Loop_CW_CCW_1
1202     '
1203 *Loop_CW_CCW_S
1204     fnAutoScreenComment(530)    '��ԕ\��[�H���U�̓���I���҂�] 2022/04/26 �n��
1205 'Ver 0.4 �ǉ� -----------------------
1206 '���H����12912 = 1 ���o�͂�����A�ēx �H��6���̓��쒆�Ď����s��
1207     MRtn = 0
1208     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec �H��6�̃t���OON�Ď�
1209     If MRtn = 1 Then M_Out(12912) = 0                  '���u����t���O��� �H��6�D��̂���12912=0���o��
1210     If MRtn = 1 Then Dly 0.7
1211     If MRtn = 1 Then GoTo *Loop_CW_CCW_S        '�H��6�̓��쒆�t���O��ON���Ă�����ēx���[�v
1212     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1213 'Ver 0.4 �����܂� -------------------
1214 '
1215     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1216     Mov PMechaSet_3             'DVD���J���u�����_1
1217 'Dly 5.0   '�f�o�b�O�p
1218 '
1219 *Loop_CW_CCW_2
1220 'Ver 0.4 �ǉ� -----------------------
1221     '���H����12912 = 1 ���o�͂�����A�ēx �H��6���̓��쒆�Ď����s��
1222     MRtn = 0
1223     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec �H��6�̃t���OON�Ď�
1224     If MRtn = 1 Then M_Out(12912) = 0                  '���u����t���O��� �H��6�D��̂���12912=0���o��
1225     If MRtn = 1 Then Dly 0.7
1226     If MRtn = 1 Then GoTo *Loop_CW_CCW_2        '�H��6�̓��쒆�t���O��ON���Ă�����ēx���[�v
1227     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1228 'Ver 0.4 �����܂� -------------------
1229 '
1230     Mov PMechaSet_2             'DVD���J���u�����_2
1231 '
1232 '    *Loop_CW_CCW_2  '���u����̏�Ԃ�������x�m�F����(�R�����g�A�E�g2/27����)
1233 '    If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_2 Else GoTo *Loop_CW_CCW_2
1234 '    *Next_CW_CCW_2
1235 '    If M_In(11920) = 0 Then GoTo *OK_FLG_2 Else GoTo *Loop_CW_CCW_2         'BaseUnit6�����u����t���O�m�F
1236 '    *OK_FLG_2
1237 ''
1238 '    M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1239 '
1240     '
1241     *RE_MECHA_SET_1
1242     If M_20# = MContinue% Then M_20# = MClear%
1243     Ovrd 25
1244     M_Out(12261) = 0            'DVD���J�V�����_�[��OFF
1245     M_Out(12260) = 1            'DVD���J�V�����_�[�oON
1246 '    Wait M_In(11271) = 1        'DVD���J�V�����_�[�o���o
1247     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�o���o
1248     If MRtn = 1 Then GoTo *CompMechaSet1
1249     Mov PMechaSet_3
1250     Mov PMechaGet_4
1251     fErrorProcess(11,271,284,0)
1252     If M_20# = MNext% Then M_20# = MClear%
1253     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1254     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1255     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1256     Mov PMechaSet_3
1257     Mov PMechaSet_2
1258     Ovrd 100
1259     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_1
1260     *CompMechaSet1
1261     '
1262     *RE_MECHA_SET_12
1263     Fine 0.05 , P
1264 '    Wait M_In(11920) = 0        'BaseUnit6�����u����t���O�m�F(�R�����g�A�E�g2/27����)
1265 '    M_Out(12912) = 1            '���u����t���O����
1266     If M_In(11931) = 1 Then     '��]�����̊m�F(CW����)(�ǉ������܂�10/1����)
1267         Mov PMechaSet1_1        'DVD���J���u�����1
1268         Ovrd 10
1269         Mvs PMechaSet1          'DVD���J���u���ʒu1
1270         Dly 0.1
1271         M_Out(12258) = 0        'DVD���J�`���b�N��OFF
1272         M_Out(12259) = 1        'DVD���J�`���b�N�JON
1273 '        Wait M_In(11268) = 1    'DVD���J�`���b�N�J���o
1274         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1275         Mvs PMechaSet1_1        'DVD���J���u�����1
1276     ElseIf M_In(11930) = 1 Then '��]�����̊m�F(CCW����)(�ǉ���������10/1����)
1277         Mov PMechaSet2_1        'DVD���J���u�����
1278         Ovrd 10
1279         Mvs PMechaSet2          'DVD���J���u���ʒu2
1280         Dly 0.1
1281         M_Out(12258) = 0        'DVD���J�`���b�N��OFF
1282         M_Out(12259) = 1        'DVD���J�`���b�N�JON
1283 '        Wait M_In(11268) = 1    'DVD���J�`���b�N�J���o
1284         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1285         Mvs PMechaSet2_1        'DVD���J���u�����2
1286     'Else
1287         '�G���[��(���u���䂪����Ȉʒu�ɖ���)
1288     EndIf                       '�ǉ������܂�10/1����
1289     Fine 0 , P
1290     '
1291     If MRtn = 1 Then GoTo *CompMechaSet2
1292     fErrorProcess(11,270,284,0)
1293     If M_20# = MNext% Then M_20# = MClear%
1294     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1295     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1296     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_12
1297     *CompMechaSet2
1298     '
1299     Ovrd 100
1300     *RE_MECHA_SET_2
1301     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1302     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1303 '    Wait M_In(11270) = 1        'DVD���J�V�����_�[�ߌ��o
1304     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�ߌ��o
1305     If MRtn = 1 Then GoTo *CompMechaSet3
1306     fErrorProcess(11,272,284,0)
1307     If M_20# = MNext% Then M_20# = MClear%
1308     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1309     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1310     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_2
1311     *CompMechaSet3
1312     '
1313     Mov PMechaSet_2             'DVD���J���u�����_2
1314     M_Out(12912) = 0                  '���u����t���O���(�ǉ�10/1����)
1315     '
1316     '�˂����{2�̐��i�����
1317     Mov PProductOnRoboGet_4     '�o�H3����4��
1318     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1319     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1320 '    Wait M_In(11876) = 1        '�˂����{2�����ҋ@����M
1321 MRtn = fScrewTighenRoboCheck(11876)    '��~��Ԃ���M����
1322 If MRtn = 0 Then Mov PInitialPosition   '"�C�j�V�����ɖ߂铮��"
1323 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1324 '
1325     *RE_ROBO_GET_1
1326 '
1327     M_Out(12259) = 0            'DVD���J�`���b�N�JOFF
1328     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1329     If M_20# = MContinue% Then Dly 0.5
1330 '
1331 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
1332     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1333     If MRtn = 1 Then GoTo *CompRoboGet1
1334     fErrorProcess(11,269,284,0)
1335     If M_20# = MNext% Then M_20# = MClear%
1336     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1337     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1338     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1339     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1340     *CompRoboGet1
1341     '
1342 '    Ovrd 50
1343     Mov PProductOnRoboGet_3     '�˂����{���i�������_2����3��
1344 '    Ovrd 20
1345     Mvs PProductOnRoboGet_2     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����2��
1346     Ovrd 20
1347     Mvs PProductOnRoboGet_1     '�˂����{���i�����
1348     Ovrd 10
1349     Mvs PProductOnRoboGet       '�˂����{���i���ʒu
1350     Dly 0.2
1351 '
1352     *RE_ROBO_GET_2
1353 '
1354     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1355     M_Out(12256) = 1            '�{�̃`���b�N��ON
1356 '
1357 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
1358     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
1359     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
1360     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1361     M_Out(12257) = 1            '�{�̃`���b�N�JON
1362     Dly 2.0
1363     Mvs PProductOnRoboGet_1
1364     Mvs PProductOnRoboGet_2
1365     Mov PProductOnRoboGet_3
1366     Mov PProductOnRoboGet_4
1367     Mov PInitialPosition
1368     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1369     M_Out(12256) = 1            '�{�̃`���b�N��ON
1370     Dly 1.0
1371     fErrorProcess(11,245,284,0)
1372     If M_20# = MNext% Then MRtn = 1
1373     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1374     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1375     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1376     M_Out(12257) = 1            '�{�̃`���b�N�JON
1377     Dly 2.0
1378     Mov PProductOnRoboGet_4
1379     Mov PProductOnRoboGet_3
1380     Mov PProductOnRoboGet_2
1381     Mvs PProductOnRoboGet_1
1382     Mvs PProductOnRoboGet
1383     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
1384     *CompRoboGet2
1385     M_20# = MClear%
1386     '
1387     Dly 0.2
1388     Mvs PProductOnRoboGet_1     '�˂����{���i�����
1389     Ovrd 50
1390     Mvs PProductOnRoboGet_2     '�˂����{���i�������_(9/27�b��R�����g�A�E�g)12/15�R�����g����
1391     Mvs PProductOnRoboGet_3     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����3��
1392     Ovrd 100
1393     Mov PProductOnRoboGet_4     '�o�H3����4��
1394     Cnt 1 , 10 , 10
1395 '
1396     M_Out(12868) = 1 Dly 0.3    '�˂����{2���슮���𑗐M
1397     *RE_ROBO_GET_3
1398     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1399     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1400 '    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1401     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1402     If MRtn = 1 Then GoTo *CompRoboGet3
1403     fErrorProcess(11,270,284,0)
1404     If M_20# = MNext% Then M_20# = MClear%
1405     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1406     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1407     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1408     *CompRoboGet3
1409     '
1410     '�p���b�g�֐��i��u��
1411     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1412     Cnt 1 , 10
1413     Mov PProductOnPltSet_1      '�{�̒u���ʒu���
1414     Cnt 0
1415     Ovrd 10
1416     Mvs PProductOnPltSet        '�{�̒u���ʒu
1417     Dly 0.5
1418 '
1419     *RE_PLT_SET
1420 '
1421     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1422     M_Out(12257) = 1            '�{�̃`���b�N�JON
1423 '
1424     Wait M_In(11265) = 1        '�{�̃`���b�N�J���o
1425 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1426     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1427     If MRtn = 1 Then GoTo *CompPltSet
1428     fErrorProcess(11,244,284,0)
1429     If M_20# = MNext% Then M_20# = MClear%
1430     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1431         Mvs PProductOnPltSet_1
1432         Mov PProductOnPltSet_2
1433         Mov PInitialPosition
1434     EndIf
1435     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1436     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1437     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1438     *CompPltSet
1439 '
1440     Mvs PProductOnPltSet_1      '�{�̒u���ʒu���
1441     Ovrd 100
1442     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1443 '    Mov PInitialPosition        '�C�j�V�����|�W�V����
1444     MRtn = FnCtlValue2(2)          '�g���n�j�{�P  2022/04/28 �n��
1445     Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
1446     MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1447     '
1448     '�`�P�b�gID��������
1449     M_20# = MAssyOK%
1450     *ASSY_ERROR_END
1451     *AssyEnd
1452     *fnAssyStart_FEndPosi
1453 FEnd
1454 '
1455 '��fnPiasCheck
1456 ''' <summary>
1457 ''' PIAS�`�P�b�g�Ǎ���
1458 ''' </summary>
1459 ''' <returns>   0 : NG
1460 '''             1 : OK(�Ǎ��݊���)
1461 ''' </returns>
1462 ''' <remarks>
1463 ''' Date   : 2021/07/07 : M.Hayakawa
1464 ''' </remarks>'
1465 Function M% fnPiasCheck
1466     fnPiasCheck = 0
1467     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1468     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1469 '
1470 *RETRY_PIAS
1471     M_20# = MClear%
1472     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1473     '
1474     '�yID�`�P�b�g�ǂݍ��݁z
1475     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1476     MInspGroup%(1) = 1              '����G�ԍ�
1477     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1478 '
1479     '�G���[�̏ꍇ
1480     If MRtn <> 1 Then
1481         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1482         If MRtn <> 1 Then
1483             'D720 -> D1300 �R�s�[�v��
1484             M_Out(12565) = 1
1485             Dly 0.5
1486             M_Out(12565) = 0
1487             '�G���[�����L�q
1488             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1489             'GOT KEY���͑҂�
1490             MKeyNumber = fnKEY_WAIT()
1491             '
1492             Select MKeyNumber
1493                 Case MNext%         '���ւ�I�������ꍇ
1494                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1495                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1496                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1497                     Break
1498                 Case MAbout%        '��~��I�������ꍇ
1499                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1500                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1501                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1502                     Break
1503                 Case MNgProcess%    'NG��I�������ꍇ
1504                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1505                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1506                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1507                     Break
1508                 Case MContinue%     '�p����I�������ꍇ
1509                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1510                     M_20# = MContinue%
1511                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1512                     Break
1513             End Select
1514         EndIf
1515     EndIf
1516 '----------D720 -> D1300 �R�s�[�v��----------
1517     M_Out(12565) = 1
1518     Dly 0.5
1519     M_Out(12565) = 0
1520 '----------�ʐM�m�F������----------
1521     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1522     MRtn = 0                ' ������
1523     M_20# = MClear%         ' ������
1524     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1525     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1526     If MRtn <> 1 Then
1527         If M_20# = MContinue% Then
1528             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1529         Else
1530             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1531         EndIf
1532     EndIf
1533 '----------�H�������m�F----------
1534     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1535     MRtn = 0                ' ������
1536     M_20# = MClear%         ' ������
1537     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1538     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1539     If MRtn <> 1 Then
1540         If M_20# = MContinue% Then
1541             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1542         Else
1543             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1544         EndIf
1545     EndIf
1546     '
1547     fnPiasCheck = 1
1548     *fnPiasCheck_End
1549 FEnd
1550 '
1551 '��fnPCComuCheck
1552 ''' <summary>
1553 ''' PC-PLC�ʐM�`�F�b�N
1554 ''' </summary>
1555 ''' <returns>   0 : NG
1556 '''             1 : OK(�Ǎ��݊���)
1557 ''' </returns>
1558 ''' <remarks>
1559 ''' Date   : 2021/07/07 : M.Hayakawa
1560 ''' </remarks>'
1561 Function M% fnPCComuCheck
1562     fnPCComuCheck = 0
1563     MJudge% = 0                                  '������
1564     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1565     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1566     '
1567     For MStaNo = 0 To 5
1568         '
1569         If M_In(MIN_PIAS_ComOK%) = 1 Then
1570             'PC�ʐMOK(M400)
1571             MJudge% = MOK%
1572             MStaNo = 5
1573             Break
1574         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1575             'toRBT_�ʐM�m�Ftime out
1576             MJudge% = MNG%
1577             MCommentD1001 = 15
1578             MCommentD1002 = 21
1579             MStaNo = 5
1580             Break
1581         Else
1582             'toRBT_�ʐM�m�Ftime out
1583             MJudge% = MNG%
1584             MCommentD1001 = 14
1585             MCommentD1002 = 21
1586             Break
1587         EndIf
1588     Next MStaNo
1589     '
1590     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1591     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1592     '
1593     '�G���[���
1594     If MJudge% <> MOK% Then
1595         M_20# = MClear%     '������
1596         '�G���[�����L�q
1597         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1598         'GOT KEY���͑҂�
1599         MKeyNumber = fnKEY_WAIT()
1600         '
1601         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1602             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1603             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1604             Break
1605         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1606             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1607             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1608             Break
1609         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1610             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1611             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1612             Break
1613         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1614             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1615             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1616             Break
1617         EndIf
1618     Else
1619         'OK�̏ꍇ
1620         fnPCComuCheck = 1
1621     EndIf
1622 FEnd
1623 '
1624 '��fnProcessCheck
1625 ''' <summary>
1626 ''' �H�������m�F
1627 ''' </summary>
1628 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1629 '''             -1�F�O�H������NG  -2�F���H����������
1630 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1631 '''             -5�F���������G���[
1632 ''' </returns>
1633 ''' <remarks>
1634 ''' Date   : 2021/07/07 : M.Hayakawa
1635 ''' </remarks>'
1636 Function M% fnProcessCheck
1637     fnProcessCheck = 0
1638     MJudge% = MNG%      '��UNG���������Ƃ���
1639 '----------�H�������m�F----------
1640     MCommentD1001 = 0   '�R�����g������
1641     For MStaNo = 0 To 5
1642         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1643         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1644         '
1645         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1646             MJudge% = MOK%
1647             fnAutoScreenComment(85)     ' AUTO���
1648             MStaNo = 5
1649             Break
1650         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1651             MFlgLoop% = 0
1652             MJudge% = MNG%
1653             MCommentD1001 = 27
1654             MCommentD1002 = 22
1655             fnAutoScreenComment(94)     ' AUTO���
1656             fnProcessCheck = -2         ' NG��-2��Ԃ�
1657             MStaNo = 5
1658             Break
1659         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1660            MJudge% = MNG%
1661             MCommentD1001 = 31
1662             MCommentD1002 = 22
1663             fnAutoScreenComment(83)     ' AUTO���
1664             fnProcessCheck = -3         ' NG��-3��Ԃ�
1665             MStaNo = 5
1666             Break
1667         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1668             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1669             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1670             MJudge% = MNG%
1671             MCommentD1001 = 32
1672             MCommentD1002 = 22
1673             fnAutoScreenComment(84)     ' AUTO���
1674             fnProcessCheck = -1         ' NG��-1��Ԃ�
1675             Dly 1.0
1676             '�H�������m�FOFF
1677             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1678             Dly 1.0
1679            'MStaNo = 5
1680             Break
1681         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1682             MFlgLoop% = 0
1683             MJudge% = MNG%
1684             MCommentD1001 = 29
1685             MCommentD1002 = 22
1686             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1687             fnProcessCheck = -5         ' NG��-5��Ԃ�
1688             MStaNo = 5
1689             Break
1690         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1691             MJudge% = MNG%
1692             If MCommentD1001 = 32 Then
1693                 '�������Ȃ�
1694             Else
1695                 MCommentD1001 = 26
1696             EndIf
1697             MCommentD1002 = 22
1698             fnProcessCheck = -4         ' NG��-4��Ԃ�
1699             MStaNo = 5
1700             Break
1701         Else
1702             MJudge% = MNG%
1703             MCommentD1001 = 28
1704             MCommentD1002 = 22
1705         EndIf
1706     Next MStaNo
1707     '�H�������m�FOFF
1708     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1709     '�ʉߗ���NG �H�������̏ꍇ
1710     If MJudge% = MPass% Then
1711         M_20# = MPass%
1712     EndIf
1713     '
1714     '�G���[���
1715     If MJudge% <> MOK% Then
1716         M_20# = MClear%     '������
1717         '�G���[�����L�q
1718         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1719         'GOT KEY���͑҂�
1720         MKeyNumber = fnKEY_WAIT()
1721         '
1722         Select MKeyNumber
1723             Case MAbout%        '��~��I�������ꍇ
1724                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1725                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1726                 Break
1727             Case MNext%         '���ւ�I�������ꍇ
1728                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1729                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1730                 Break
1731             Case MContinue%     '�p����I�������ꍇ
1732                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1733                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1734                 Break
1735             Case MNgProcess%    'NG��I�������ꍇ
1736                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1737                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1738                 Break
1739         End Select
1740     Else
1741         fnProcessCheck = 1  ' OK��1��Ԃ�
1742     EndIf
1743 FEnd
1744 '
1745 '��fnPiasWrite
1746 ''' <summary>
1747 ''' Pias �g�����ʏ����ݗv��
1748 ''' </summary>
1749 '''<param name="MFlg%">
1750 '''                 MOK%(1) = �H��������OK��������
1751 '''                 MNG%(0) = �H��������NG��������
1752 '''</param>
1753 '''<returns></returns>
1754 ''' <remarks>
1755 ''' Date   : 2021/07/07 : M.Hayakawa
1756 ''' </remarks>'
1757 Function M% fnPiasWrite(ByVal MFlg%)
1758       fnPiasWrite = 0
1759 *RETRY_PIASWRITE
1760     '
1761     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1762    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1763     If MFlg% = MOK% Then
1764         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1765     Else
1766         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1767     EndIf
1768     Dly 0.1                  '�O�̂���
1769     '
1770     'Pias�֏����݊J�n M305 -> ON
1771     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1772     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1773     '
1774     MJudge% = MNG%
1775     '
1776     For MStaNo = 0 To 5
1777         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1778             MJudge% = MOK%
1779             'MRet = fnAutoScreenComment(85)  'AUTO���
1780             MStaNo = 5
1781             Break
1782         '
1783         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1784             MJudge% = MNG%
1785             'MRet = fnAutoScreenComment(85)  'AUTO���
1786            MCommentD1001 = 34
1787            MCommentD1002 = 25
1788             MStaNo = 5
1789             Break
1790         '
1791         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1792             MJudge% = MNG%
1793             'MRet = fnAutoScreenComment(85)  'AUTO���
1794            MCommentD1001 = 35
1795            MCommentD1002 = 25
1796             MStaNo = 5
1797             Break
1798         '
1799         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1800             MJudge% = MNG%
1801             'MRet = fnAutoScreenComment(85)  'AUTO���
1802            MCommentD1001 = 36
1803            MCommentD1002 = 25
1804             MStaNo = 5
1805             Break
1806         '
1807         Else
1808             MJudge% = MNG%
1809            MCommentD1001 = 42
1810            MCommentD1002 = 25
1811         '
1812         EndIf
1813         '
1814     Next MStaNo
1815     '
1816     'Pias�֏����݊J�n M305 -> OfF
1817     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1818     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1819     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1820     '
1821     '
1822     '�ʉߗ���NG �H�������̏ꍇ
1823     If MJudge% = MPass% Then
1824         M_20# = MPass%
1825     EndIf
1826     '
1827    M_20# = MClear%     '������
1828     '
1829     '�G���[���
1830     If MJudge% < MOK% Then
1831     '
1832 '�c���Ă���������ł͎g�p���Ȃ����x��
1833 *RETRY_ERR_WRITE
1834         M_20# = MClear%     '������
1835         '�G���[�����L�q
1836         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1837         'GOT KEY���͑҂�
1838         MKeyNumber = fnKEY_WAIT()
1839         '
1840         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1841             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1842            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1843             Break
1844         '
1845         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1846             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1847             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1848         '
1849         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1850             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1851             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1852         '
1853         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1854             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1855            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1856             Break
1857         '
1858         EndIf
1859         '
1860         If M_20# = MClear% Then *RETRY_ERR_WRITE
1861         '
1862     EndIf
1863     '
1864     If M_20# = MContinue% Then *RETRY_PIASWRITE
1865     '
1866     fnPiasWrite = 1
1867     '
1868 FEnd
1869 '
1870 '��fnPCBNumberCheck
1871 ''' <summary>
1872 ''' Pias ��ԍ��ƍ��v��
1873 ''' </summary>
1874 '''<param name="%"></param>
1875 '''<param name="%"></param>
1876 '''<returns></returns>
1877 ''' <remarks>
1878 ''' Date   : 2021/07/07 : M.Hayakawa
1879 ''' </remarks>'
1880 Function M% fnPCBNumberCheck
1881       fnPCBNumberCheck = 0
1882     '
1883 *RETRY_PCBCHECK
1884     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1885     'Pias�֊�ƍ��J�n M310 -> ON
1886     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1887     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1888     '
1889     MJudge% = MNG%
1890     '
1891     For MStaNo = 0 To 5
1892         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1893             MJudge% = MOK%
1894             fnAutoScreenComment(96)  'AUTO���
1895             MStaNo = 5
1896             Break
1897         '
1898         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1899             MJudge% = MNG%
1900             fnAutoScreenComment(97)  'AUTO���
1901             MCommentD1001 = 37
1902             MCommentD1002 = 25
1903             MStaNo = 5
1904             Break
1905         '
1906         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1907             MJudge% = MNG%
1908             fnAutoScreenComment(98)  'AUTO���
1909             MCommentD1001 = 38
1910             MCommentD1002 = 25
1911             MStaNo = 5
1912             Break
1913         '
1914         ElseIf M_In(11580) = 1 Then                         'time out
1915             MJudge% = MNG%
1916             fnAutoScreenComment(99)  'AUTO���
1917             MCommentD1001 = 39
1918             MCommentD1002 = 25
1919             MStaNo = 5
1920             Break
1921         '
1922         Else
1923             MJudge% = MNG%
1924            MCommentD1001 = 41
1925            MCommentD1002 = 25
1926         '
1927         EndIf
1928         '
1929     Next MStaNo
1930     '
1931     'Pias�֊�ƍ��J�n M310 -> OfF
1932     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1933     '
1934     '
1935     '�ʉߗ���NG �H�������̏ꍇ
1936     If MJudge% = MPass% Then
1937         M_20# = MPass%
1938     EndIf
1939     '
1940    M_20# = MClear%     '������
1941     '
1942     '�G���[���
1943     If MJudge% < MOK% Then
1944     '
1945 '�c���Ă���������ł͎g�p���Ȃ����x��
1946 *RETRY_ERR_PCBNUMBER
1947         M_20# = MClear%     '������
1948         '�G���[�����L�q
1949         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1950         'GOT KEY���͑҂�
1951         MKeyNumber = fnKEY_WAIT()
1952         '
1953         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1954             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1955             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1956             Break
1957         '
1958         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1959             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1960             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1961         '
1962         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1963             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1964             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1965         '
1966         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1967             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1968             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1969             Break
1970         '
1971         EndIf
1972         '
1973         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1974         '
1975     EndIf
1976     '
1977     If M_20# = MContinue% Then *RETRY_PCBCHECK
1978 FEnd
1979 '
1980 '��ScrewTight_S2
1981 ''' <summary>
1982 ''' �˂����߂��s��
1983 ''' </summary>
1984 '''<param name="PScrewPos()">
1985 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1986 '''             PScrewPos(2)    �F�˂����߉��_
1987 '''             PScrewPos(10)   �F�˂����ߏI������
1988 '''</param>
1989 '''<returns>����
1990 '''         0=�ُ�I���A1=����I��
1991 '''</returns>
1992 ''' <remarks>
1993 ''' Date   : 2021/07/07 : M.Hayakawa
1994 ''' </remarks>'
1995 Function M% ScrewTight_S2(ByVal PScrewPosition())   '�l�W���ߌʐݒ�
1996     ScrewTight_S2 = 0
1997     MOKNGFlg = 0
1998     Ovrd 100
1999     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
2000     ' �b��
2001     Ovrd 5
2002     Mvs PScrewPosition(10),-10    ' �p���b�g��˂�����S�@�̏��ֈړ�
2003 '    Ovrd MOvrdA
2004     '�b��}�X�N
2005 '    M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
2006 '    Dly 0.1
2007 '    M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
2008 '    'Spd 8.3 '�O�����C�h100  �������C�h60   '���C�h100-40�@100%�FSpd�@15�@'�˂����ߑ��x�ݒ�
2009 '    Spd MSpdA               '�l�W���ߎ�Spd�ʐݒ�
2010     ' �b��ړ��̂�
2011     Mvs PScrewPosition(10)
2012 '    '
2013 '    Dly 0.1
2014 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
2015 '    Wait M_In(11584)=1          '����/�G���[���o
2016 '    Dly 0.1
2017 '    Spd M_NSpd
2018 '    '
2019 '    If M_In(X28_Driver)=1 Then  '�˂��g�[�^���G���[���o��
2020 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
2021 '        Dly 0.1
2022 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
2023 '        Dly 0.1
2024 '        M_Out(Y63_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
2025 '        Dly 0.1
2026 '        M_Out(Y65_Driver)=0     '�v���O���������@F1
2027 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
2028 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
2029 '        MOKNGFlg = -1
2030 '        ScrewTight_S2 = 0
2031 '    Else
2032 '        Wait M_In(X29_Driver)=1 ' ���튮����
2033 '        Dly 0.1
2034 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
2035 '        Dly 0.1
2036 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O����
2037 '        Dly 0.1
2038 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
2039 '        Dly 0.1
2040 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
2041 '        ScrewTight_S2 = 1
2042 '    EndIf
2043 ' �b��
2044     Ovrd 10
2045     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
2046     Ovrd 100
2047 FEnd
2048 '
2049 '��ScrewGet_S3
2050 ''' <summary>
2051 ''' �˂������@����˂��𓾂�
2052 ''' </summary>
2053 '''<param name="%"></param>
2054 '''         PScrewPos(1)    �F�˂�������̂˂����
2055 '''         PScrewPos(2)    �F�˂���������_
2056 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2057 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
2058 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
2059 '''<returns>����
2060 '''         0=�ُ�I���A1=����I���A-1=M�l�W�Z���T�[NG�A-2=M�l�W�Z���T�[ON�A-3=�z���G���[
2061 '''</returns>
2062 ''' <remarks>
2063 ''' Date   : 2021/07/07 : M.Hayakawa
2064 ''' </remarks>'
2065 Function M% ScrewGet_S3(ByVal PScrewPosition())
2066     ScrewGet_S3 = 0
2067     MMScrewJudge% = 0
2068     '�˂������평������G���[�`�F�b�N
2069 ' ���b��폜
2070 '    Wait M_In(X34_ScrewReady1)=1 '�˂�������S��Ready�ɂȂ�܂ő҂@���@���b���҂���Ready�ɂȂ�Ȃ���Δ�����v���O�������K�v�H
2071 '    Ovrd 100
2072 '    If M_In(X33_SS2)=0 Then  'M�˂����o�Z���T��OFF�i�̏�j���Ă����ꍇ
2073 '        Ovrd 30
2074 '        Mvs,-80             '���̏ꏊ����80mm���ֈړ�
2075 '        Mov PInitPos19049   '19049�����ʒu�ֈړ�
2076 '        M_Out(Y6A_VV1)=0    '�˂��z�� Off
2077 '        'NG�Ƃ��Ă����̊֐����甲����
2078 '        ScrewGet_S3 = -1
2079 '        MMScrewJudge% = 1
2080 '        MCommentD1001 = 61
2081 '    EndIf
2082 '    If ScrewGet_S3 = 0 Then
2083 '        'S�^�C�g�p�˂������@��M�˂����������Ă��Ȃ����Ď�
2084 '        MMScrewJudge% = 0 'MMScrewJudge������������
2085 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
2086 '        If MRtn = 0 Then
2087 '            Ovrd 30
2088 '            Mvs,-80            '���̏ꏊ����50mm���ֈړ�
2089 '            Mov PInitPos19049  '19049�����ʒu�ֈړ�
2090 '            MMScrewJudge% = 2
2091 '            MRtn = All_CLamp_Release()'�S�ẴN�����v�����֕���
2092 '            MCnt% = 2   '2��ݒ�
2093 '            MCommentD1001 = 62
2094 '        EndIf
2095 '        If MMScrewJudge% = 2 Then
2096 '            ScrewGet_S3 = -2
2097 '        EndIf
2098 '    EndIf
2099 '    'M�l�W���肪ON�̏ꍇ NG�Ƃ��Ċ֐��𔲂���
2100 '    If MMScrewJudge% = 2 Then
2101 '        ScrewGet_S3 = -2
2102 '    EndIf
2103     'S�l�W�p�˂����Y��M�l�W�����m�F�p�����܂�
2104     Ovrd 100
2105     Spd M_NSpd
2106     If MMScrewJudge% = 0 Then
2107         ScrewGet_S3 = 0
2108         M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
2109         MScrewCnt% = 0
2110         MFinCnt% = 2
2111 '        For MCnt% = 0 To MFinCnt%
2112             Mov PScrewPosition(2)        ' �˂������@���_
2113             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
2114             Ovrd 80
2115             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2116             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
2117             Mvs PScrewPosition(10), 1.2
2118             M_Out(Y6A_VV1)=1        ' �˂��z���@ON
2119             '�r�b�g��]
2120             M_Out(Y60_Driver)=1
2121             Dly 0.2
2122             '
2123             Ovrd 100
2124             JOvrd M_NJovrd
2125             Spd M_NSpd
2126             '�l�W�z���m�F�ʒu�ړ�
2127             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
2128             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
2129             '�r�b�g��]��~
2130             'M_Out(Y60_Driver)=0
2131             '
2132             '1�b�ԃl�W�z���m�F
2133 ' �ȉ��b��폜
2134 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2135 '            'MRtn = 0'�����G���[
2136 '            '�z���G���[�̏ꍇ
2137 '            '�l�W���˂����Y�ɖ߂�
2138 '            If MRtn = 0 Then
2139 '                Ovrd 30
2140 '                '�r�b�g��]��~
2141 '                M_Out(Y60_Driver)=0
2142 '                '�l�W�����@���
2143 '                Mvs PScrewPos(1)
2144 '                '�X�ɏ��
2145 '                Mov PScrewPos(1), -75
2146 '                '�l�W�̂Ĉʒu
2147 '                Mov PScrewFeedS021
2148 '                '�z��OFF
2149 '                M_Out(Y6A_VV1)=0 '�˂��z���@OFF
2150 '                Dly 0.2
2151 '                '�j��ON
2152 '                M_Out(Y6B_VB1)=1 '�^��j��ON
2153 '                '�r�b�g��]
2154 '                M_Out(Y61_Driver)=1
2155 '                Dly 0.5
2156 '                '
2157 '                Ovrd 100
2158 '                JOvrd M_NJovrd
2159 '                Spd M_NSpd
2160 '                '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
2161 '                Mov PScrewFeedS021, 10
2162 '                Mov PScrewFeedS021
2163 '                Dly 0.1
2164 '                Mov PScrewFeedS021, 10
2165 '                Mov PScrewFeedS021
2166 '                '
2167 '                '�l�W�����҂�
2168 '                '�r�b�g��]��~
2169 '                M_Out(Y61_Driver)=0
2170 '                Dly 0.1
2171 '                '�j��OFF
2172 '                M_Out(Y6B_VB1)=0 '�^��j��OFF
2173 '                '
2174 '                '
2175 '                '�˂��������Ƃ��āA�ړ��X�ɏ��
2176 '                Mov PScrewPos(1), -75
2177 '                Ovrd 100
2178 '                Spd M_NSpd
2179 '                '�l�W�����@���
2180 '                Mvs PScrewPos(1)
2181 '                '
2182 '                ScrewGet_S3 = -3
2183 '                Break
2184 '                '
2185 '            Else
2186 '                MCnt% = MFinCnt%
2187 '                ScrewGet_S3 = 0
2188 '            EndIf
2189 '        Next  MCnt%
2190         '
2191         Ovrd 100
2192         Spd M_NSpd
2193         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2194         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
2195         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
2196         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2197         '������x�z���m�F
2198 ' �ȉ��b��폜
2199 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2200 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
2201 '            MCommentD1001 = 94
2202 '            MCommentD1002 = 95
2203 '            ScrewGet_S3 = -3
2204 '        EndIf
2205 '        If MRtn = 1 Then      '�z��OK�̏ꍇ
2206 '            ScrewGet_S3 = 1
2207 '        EndIf
2208 '        Break
2209     Else
2210         'M�l�W
2211         If MMScrewJudge% = 2 Then
2212             ScrewGet_S3 = -2
2213         EndIf
2214     EndIf
2215 FEnd
2216 '
2217 '��fnKEY_WAIT()
2218 ''' <summary>
2219 ''' GOT����̃L�[���͑҂�
2220 ''' </summary>
2221 '''<returns>1�F��~    2�F����
2222 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
2223 '''         5�FNG
2224 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
2225 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
2226 '''</returns>
2227 ''' <remarks>
2228 ''' Date   : 2021/07/07 : M.Hayakawa
2229 ''' </remarks>'
2230 Function M% fnKEY_WAIT()
2231     fnKEY_WAIT = 0
2232     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
2233     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
2234     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
2235     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2236     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
2237     Dly 0.2
2238     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
2239     MLocalLoopFlg=1
2240     While MLocalLoopFlg=1
2241         If M_In(11345) = 1 Then         '��~   M5345
2242             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
2243             fnKEY_WAIT = 1
2244             MLocalLoopFlg=-1
2245             Break
2246         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
2247             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
2248             fnKEY_WAIT = 2
2249             MLocalLoopFlg=-1
2250             Break
2251         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
2252             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
2253             fnKEY_WAIT = 3
2254             MLocalLoopFlg=-1
2255             Break
2256         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
2257             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
2258             fnKEY_WAIT = 4
2259             MLocalLoopFlg=-1
2260             Break
2261         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
2262             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
2263             fnKEY_WAIT = 5
2264             MLocalLoopFlg=-1
2265             Break
2266             '
2267         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
2268             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
2269             fnKEY_WAIT = MRobotInit1%
2270             MLocalLoopFlg=-1
2271             Break
2272             '
2273         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
2274             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
2275             fnKEY_WAIT = MRobotInit2%
2276             MLocalLoopFlg=-1
2277             Break
2278             '
2279         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
2280             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
2281             fnKEY_WAIT = MRobotInit3%
2282             MLocalLoopFlg=-1
2283             Break
2284             '
2285         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2286             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2287             fnKEY_WAIT = MRobotInit4%
2288             MLocalLoopFlg=-1
2289             Break
2290             '
2291         Else
2292         EndIf
2293     WEnd
2294     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2295     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2296 FEnd
2297 '
2298 '�� fnAUTO_CTL
2299 ''' <summary>
2300 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2301 ''' </summary>
2302 ''' <remarks>
2303 ''' Date   : 2021/07/07 : M.Hayakawa
2304 ''' </remarks>
2305 Function M% fnAUTO_CTL
2306     fnAUTO_CTL = 0
2307     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2308     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2309     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2310     '
2311     If M_Svo=0 Then             '�T�[�{ON�m�F
2312         Servo On
2313     EndIf
2314     Wait M_Svo=1
2315 FEnd
2316 '
2317 '�� fnWindScreenOpen
2318 ''' <summary>
2319 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2320 ''' </summary>
2321 '''<param name="%"></param>
2322 '''<param name="%"></param>
2323 '''<param name="%"></param>
2324 '''<param name="%"></param>
2325 ''' <remarks>
2326 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2327 ''' MWindReSet = 0     ��ʔ�\��
2328 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2329 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2330 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2331 ''' Date   : 2021/07/07 : M.Hayakawa
2332 ''' </remarks>
2333 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2334     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2335         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2336     EndIf
2337     '
2338     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2339         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2340     EndIf
2341     '
2342     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2343        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2344     EndIf
2345     '
2346     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2347     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2348     Dly 0.5
2349     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2350 FEnd
2351 '
2352 '��FnCtlValue2
2353 ''' <summary>
2354 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2355 ''' </summary>
2356 ''' <param name="MCtlNo%"></param>
2357 ''' <remarks>
2358 ''' Date : 2022/04/28 �n��
2359 ''' </remarks>
2360 '''
2361 '''  1�F������       �{�P
2362 '''  2�F�g���n�j��   �{�P
2363 '''  3�F�g���m�f��   �{�P (���g�p)
2364 '''  4�F�z���G���[�� �{�P
2365 ''' 99�F�Ǐ��J�n�M�� OFF
2366 '''
2367 Function M% FnCtlValue2(ByVal MCtlNo%)
2368     FnCtlValue2 = 1
2369     Select MCtlNo%
2370         Case 1        '�������{�P
2371             M_Out(12569) = 0             '�����݊J�n�M��OFF
2372             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2373             MInputQty = M_In16(11600)    '��������M
2374             MInputQty = MInputQty + 1    '�������{�P
2375             M_Out16(12592) = MInputQty   '���������M
2376             M_Out(12569) = 1             '�����݊J�n�M��ON
2377             Break
2378             '
2379         Case 2        '�g���n�j���{�P
2380             M_Out(12569) = 0             '�����݊J�n�M��OFF
2381             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2382             MAssyOkQty = M_In16(11616)   '�g��OK����M
2383             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2384             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2385             M_Out(12569) = 1             '�����݊J�n�M��ON
2386             Break
2387             '
2388         Case 4        '�z���G���[���{�P
2389             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2390             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2391             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2392             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2393             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2394             M_Out(12569) = 1                       '�����݊J�n�M��ON
2395             Break
2396             '
2397         Case 99        '�Ǐ��J�n�M��OFF
2398             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2399             M_Out(12569) = 0        '�����݊J�n�M��OFF
2400             Break
2401             '
2402     End Select
2403     Exit Function
2404 FEnd
2405 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2406 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2407 '-------------------------------------------------------------------------------
2408 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2409 '   ����
2410 '       PInspPos()      �F�����ʒu
2411 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2412 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2413 '       MInspCnt%       �F�����ʒu��
2414 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2415 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2416 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2417 '   �߂�l�F����
2418 '       0=�ُ�I���A1=����I��
2419 '
2420 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2421 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2422 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2423 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2424 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2425 '-------------------------------------------------------------------------------
2426     '----- �����ݒ� -----
2427     Cnt 0                                                           '�ړ�����������(�����l=0)
2428     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2429 '    Cnt 1,0.1,0.1
2430     '�ϐ��錾�E������
2431     Def Inte MNum                                                   '�����ԍ�(������1�`)
2432     MNum% = 1                                                       '�����ԍ������l�ݒ�
2433     Def Inte MEndFlg                                                '�����I���t���O
2434     MEndFlg% = 0
2435     '
2436     '����G�ԍ��ݒ�v���E�������s�v��off
2437     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2438     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2439     '�G���[�ԍ��N���A
2440     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2441     M_Out16(MOUT_InspErrNum) = MInspErrNum
2442     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2443     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2444     '
2445     'Insight Ready check?
2446     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2447         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2448         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2449         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2450         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2451         Exit Function
2452     EndIf
2453     '
2454     '�����ʒu���m�F
2455     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2456         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2457         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2458         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2459         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2460         Exit Function
2461     EndIf
2462     '
2463     '
2464     '
2465     '----- ���C������ -----
2466     '�ݒ肳�ꂽ�����ʒu�����̌������s
2467     While( MEndFlg% = 0 )
2468         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2469         MSetGrNumRetryExitFlg = 0
2470         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2471         While( MSetGrNumRetryExitFlg = 0 )
2472         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2473             '
2474             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2475             '
2476             '----- �����O���[�v�ԍ��ݒ� -----
2477             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2478             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2479             '
2480             '�����ʒu�ֈړ��E�ړ������҂�
2481             Mvs PInspPos( MNum% )                                       '�ړ�
2482             Dly 0.05                                                    '�ړ�������Delay
2483             '
2484             '�����O���[�v�ԍ��ݒ�I���m�F
2485             M_Timer(1) = 0
2486             MExitFlg = 0
2487             While( MExitFlg = 0 )
2488                 '����G�ݒ萳��I��?
2489                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2490                     MExitFlg = 1
2491                 '
2492                 '����G�ݒ�ُ�I��?
2493                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2494                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2495                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2496                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2497                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2498                     EndIf
2499                     MExitFlg = 1
2500                 '
2501                 'timeout�`�F�b�N
2502                 ElseIf 1000 < M_Timer(1) Then
2503                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2504                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2505                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2506                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2507                     EndIf
2508                     MExitFlg = 1
2509                 EndIf
2510             WEnd
2511             '
2512             '����G�ԍ��ݒ�v��off
2513             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2514             '
2515             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2516             'NG�Ȃ���Δ�����
2517             If MCurrentStepErr = 0 Then
2518                 MSetGrNumRetryExitFlg = 1
2519             Else
2520                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2521                 If MSetGrNumRetryCnt = 0 Then
2522                     MSetGrNumRetryExitFlg = 1
2523                 Else
2524                     'Retry�ց@���̑O��Delay
2525                     Dly 0.5
2526                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2527                 EndIf
2528             EndIf
2529             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2530             '
2531         WEnd
2532         '
2533         '
2534         '
2535         '----- �������s -----
2536         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2537             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2538                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2539                 MInspRetryExitFlg = 0
2540                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2541                 While( MInspRetryExitFlg = 0 )
2542                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2543                     '
2544                     '���������m�F
2545                     MRetryCnt = MRetryCnt - 1
2546                     M_Timer(1) = 0
2547                     MExitFlg = 0
2548                     While( MExitFlg = 0 )
2549                     '���������҂�
2550                         '����OK�I��?
2551                         If M_In( MIN_IS_InspOK% ) = 1  Then
2552                             MJudgeOKFlg = 1                         '����OK�t���OON
2553                             MExitFlg = 1
2554                         '
2555                         '����NG�I��?
2556                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2557                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2558                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2559                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2560                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2561                                 EndIf
2562                             EndIf
2563                             MExitFlg = 1
2564                         '
2565                         '�����ُ�I��(IS timeout)?
2566                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2567                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2568                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2569                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2570                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2571                                 EndIf
2572                             EndIf
2573                             MExitFlg = 1
2574                         '
2575                         'timeout�`�F�b�N
2576                         ElseIf 3000 < M_Timer(1) Then
2577                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2578                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2579                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2580                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2581                                 EndIf
2582                             EndIf
2583                             MExitFlg = 1
2584                         EndIf
2585                     WEnd
2586                     '
2587                     '�����J�n�v��off
2588                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2589                     '
2590                     'OK�Ȃ甲����
2591                     If MJudgeOKFlg = 1 Then
2592                         MInspRetryExitFlg = 1
2593                     Else
2594                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2595                         If MRetryCnt = 0 Then
2596                             MInspRetryExitFlg = 1
2597                         Else
2598                             'Retry�ց@���̑O��Delay
2599                             Dly 0.3
2600                         EndIf
2601                     EndIf
2602                     '
2603                 WEnd
2604             EndIf
2605         EndIf
2606         '
2607         '
2608         '
2609         MNum% = MNum% + 1                                           '����Step+1
2610         '�����I���m�F�@�����I���t���O�Z�b�g
2611         If (MInspCnt% < MNum% ) Then
2612             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2613         EndIf
2614         'NG���������s������
2615         If MInspErrNum <> 0 Then                                    'NG����?
2616             If MNgContinue% <> 1 Then                               'NG���s?
2617                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2618             EndIf
2619         EndIf
2620     WEnd
2621     '
2622     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2623     If 0 < MZAxis% Then
2624         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2625         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2626         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2627     EndIf
2628     Fine 0 , P
2629     '
2630     '�߂�l�ݒ�
2631     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
2632         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2633     Else
2634         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2635         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2636         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2637     EndIf
2638     '
2639 FEnd
2640 '
2641 ' ��ISInspection
2642 ''' <summary>
2643 ''' Insight�ɂ��摜�����������s
2644 ''' </summary>
2645 '''<param name="PInspPos()">�����ʒu</param>
2646 '''<param name="MInspGrNum%()">�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j</param>
2647 '''             PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2648 '''<param name="MInspCnt%">�����ʒu��</param>
2649 '''<param name="MZAxis%">�I������Z���ޔ����W�i-1:�����j</param>
2650 '''             �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2651 '''<param name="MNgContinue%">=1�Ō����G���[�ENG�������ɑSStep�̌������s��</param>
2652 '''<returns>    ���� 0=�ُ�I���A1=����I��</returns>
2653 '''         MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2654 '''         MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���"
2655 ''' <remarks>
2656 ''' Date   : 2021/07/07 : M.Hayakawa
2657 ''' </remarks>
2658 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2659 '    '�摜�g�p�m�F 0<- �摜�m�F�����̏ꍇ
2660 '    If M_In(11369) = 0 Then            'toRBT_�g�p�m�F
2661 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2662 '    EndIf
2663 ''
2664 '    Cnt 0                                                       '�ړ�����������(�����l=0)
2665 '    Fine 0.05,P                                                 '�ʒu���ߊ��������ݒu�@0.05mm
2666 '    MNum% = 1                                                   '�����ԍ������l�ݒ�
2667 '    Def Inte MEndFlg                                            '�����I���t���O
2668 '    MEndFlg% = 0
2669 '    '
2670 '    '�G���[�ԍ��N���A
2671 '    MInspErrNumSub = 0                                          '�������s�G���[�ԍ�sub
2672 '    MInspErrNum = 0                                             '�������s�G���[�ԍ�
2673 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2674 '    MInspNGStepNum = 0                                          '�������sNGStep�ԍ�
2675 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2676 '    '
2677 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready off�Ȃ�I��
2678 '        MInspErrNum = 20                                        '�������s�G���[�ԍ� 20 Insight offline
2679 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2680 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2681 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2682 ''
2683 '    EndIf
2684 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2685 '    '
2686 '    '�����ʒu���m�F
2687 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2688 '        MInspErrNum = 21                                        '�����f�[�^�Ȃ� 21�@����<1
2689 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2690 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2691 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2692 ''
2693 '    EndIf
2694 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2695 '    '
2696 '    '�ݒ肳�ꂽ�����ʒu�����̌������s
2697 '    While( MEndFlg% = 0 )
2698 '        '�����I���m�F�@�����I���t���O�Z�b�g
2699 '        If (MInspCnt% < MNum% ) Then
2700 '            MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2701 '        EndIf
2702 '        '
2703 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����J�n�@INSPTAST1
2704 '        If MEndFlg% = 0 Then
2705 '            M_01# = MInspGrNum%(MNum%)                          '����G�ԍ����n��
2706 '        EndIf
2707 '        M_02# = MEndFlg%                                        '�����I���t���O���n��
2708 '        M_05# = MNum%                                           '�����ԍ�(������1�`)
2709 '        '�^�X�N�@����G�ݒ�t���O���n��
2710 '        If MEndFlg% = 0 Then
2711 '            If 0 < MInspGrNum%(MNum%) Then
2712 '                M_03# = 1
2713 '            Else
2714 '                M_03# = 0
2715 '            EndIf
2716 '        Else
2717 '            M_03# = 0
2718 '        EndIf
2719 '        '�^�X�N�@�������ʊm�F�t���O���n��
2720 '        If 1 < MNum% Then
2721 '            If 0 < MInspGrNum%(MNum%-1) Then
2722 '                M_04# = 1
2723 '            Else
2724 '                M_04# = 0
2725 '            EndIf
2726 '        Else
2727 '            M_04# = 0
2728 '        EndIf
2729 '        '
2730 '        '�^�X�N�����J�n
2731 '        M_00# = 1                                               'TASK�����J�n
2732 '        '�^�X�N�����J�n�m�F
2733 '        M_Timer(1) = 0
2734 '        MExitFlg = 0
2735 '        While( MExitFlg = 0 )
2736 '            '�����J�n�����m�F
2737 '            If M_00# = 0 And M_10# = 8 Then
2738 '                MExitFlg = 1
2739 '            EndIf
2740 '            'timeout�`�F�b�N
2741 '            If 2000 < M_Timer(1) Then
2742 '                If MNgContinue% = 1 Then                        'NG���s?
2743 '                    MInspErrNumSub = 36                         '�G���[�ԍ��ݒ�36
2744 '                Else
2745 '                    MInspErrNum = 36                            '�G���[�ԍ��ݒ�36
2746 '                EndIf
2747 '                MExitFlg = 1
2748 '            EndIf
2749 '        WEnd
2750 '        '
2751 '        '�����ʒu�ֈړ��E�ړ������҂�
2752 '        If 0 = MInspErrNum Then
2753 '            If MEndFlg% = 0 Then
2754 '                Mvs PInspPos( MNum% )                           '�ړ�
2755 '            EndIf
2756 '        EndIf
2757 '        '
2758 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����I���҂��@INSPTAST1
2759 '        If 0 = MInspErrNum Then
2760 '            M_Timer(1) = 0
2761 '            MExitFlg = 0
2762 '            While( MExitFlg = 0 )
2763 '                '���������҂��i����I���j
2764 '                If M_10# = 1 Then
2765 '                    MExitFlg = 1
2766 '                EndIf
2767 '                '���������҂��i�ُ�I���j
2768 '                If M_10# = 0 Then
2769 '                    If MNgContinue% = 1 Then                    'NG���s?
2770 '                        MInspErrNumSub = M_12#                  '�G���[�ԍ��ݒ�@M12
2771 '                    Else
2772 '                        MInspErrNum = M_12#                     '�G���[�ԍ��ݒ�@M12
2773 '                    EndIf
2774 '                    MExitFlg = 1
2775 '                EndIf
2776 '                'timeout�`�F�b�N
2777 '                If 5000 < M_Timer(1) Then
2778 '                    If MNgContinue% = 1 Then                    'NG���s?
2779 '                        MInspErrNumSub = 31                     '�G���[�ԍ��ݒ�31
2780 '                    Else
2781 '                        MInspErrNum = 31                        '�G���[�ԍ��ݒ�31
2782 '                    EndIf
2783 '                    MExitFlg = 1
2784 '                EndIf
2785 '            WEnd
2786 '        EndIf
2787 '        '
2788 '        '�������ʊm�F
2789 '        If 0 = MInspErrNum Then
2790 '            If 1 < MNum% Then
2791 '                If 0 < MInspGrNum%(MNum%-1) Then                '��������?
2792 '                    If M_11# = 2 Then                           '����NG?
2793 '                        If MNgContinue% = 1 Then                'NG���s?
2794 '                            If MInspNGStepNum = 0 Then          'NG������?
2795 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2796 '                            EndIf
2797 '                            MInspErrNumSub = 32                 '�G���[�ԍ��ݒ� 32:����NG
2798 '                        Else
2799 ''                            MInspNGStepNum = MNum% - 1          '�������sNGStep�ԍ��ݒ�
2800 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2801 '                            MInspErrNum = 32                    '�G���[�ԍ��ݒ� 32:����NG
2802 '                        EndIf
2803 '                   EndIf
2804 '                EndIf
2805 '            EndIf
2806 '        EndIf
2807 '        '
2808 '        '�G���[�Ȃ猟�����f�I������̂�Loop���甲���邽�ߏI���t���O�Z�b�g
2809 '        If 0 <> MInspErrNum Then
2810 '            MEndFlg% = 1
2811 '        EndIf
2812 '        '
2813 '        '�������s�A�捞�����҂�
2814 '        If 0 = MInspErrNum Then
2815 '            If MEndFlg% = 0 Then
2816 '                If 0 < MInspGrNum%(MNum%) Then                  '��������?
2817 '                    M_Out(MOUT_IS_Insp%) = 1                    '�������s�v��on
2818 '                    '�捞�����m�F
2819 '                    M_Timer(1) = 0
2820 '                    MExitFlg = 0
2821 '                    While( MExitFlg = 0 )
2822 '                        '���������҂�
2823 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2824 '                            MExitFlg = 1
2825 '                        EndIf
2826 '                        'timeout�`�F�b�N
2827 '                        If 2000 < M_Timer(1) Then
2828 '                            If MNgContinue% = 1 Then            'NG���s?
2829 '                                MInspErrNumSub = 33             '�G���[�ԍ��ݒ�33
2830 '                            Else
2831 '                                MInspErrNum = 33                '�G���[�ԍ��ݒ�33
2832 '                            EndIf
2833 '                            MExitFlg = 1
2834 '                        EndIf
2835 '                    WEnd
2836 '                EndIf
2837 '                '
2838 '            EndIf
2839 '        EndIf
2840 '        MNum% = MNum% + 1
2841 '    WEnd
2842 '    '
2843 '    '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2844 '    If 0 < MZAxis% Then
2845 '        PCurrentPos = P_Curr                                    '���݈ʒu�擾
2846 '        PCurrentPos.Z = MZAxis%                                 'Z����ݒ�
2847 '        Mvs PCurrentPos                                         '���݈ʒu���ֈړ�
2848 '    EndIf
2849 '    '
2850 '    'NG���s������
2851 '    If MNgContinue% = 1 Then                                    'NG���s?
2852 '        MInspErrNum = MInspErrNumSub                            '�G���[�ԍ��ݒ�
2853 '    EndIf
2854 '    '
2855 '    '�߂�l�ݒ�
2856 '    If MInspErrNum = 0 Then
2857 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2858 '    Else
2859 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2860 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2861 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2862 '    EndIf
2863 '    '
2864 '*ISInspection_End
2865 'FEnd
2866 '
2867 '��InitialZoneB
2868 ''' <summary>
2869 ''' ����~��̕��A����
2870 ''' 1)���ޔ��@Z������Ɉړ�
2871 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
2872 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
2873 ''' 4)�C�j�V�����|�W�V�����ֈړ�
2874 ''' </summary>
2875 ''' <remarks>
2876 ''' Date : 2022/04/08 : N.Watanabe
2877 ''' </remarks>
2878 Function V fnInitialZoneB()
2879     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/04/26 �n��
2880 '
2881 '�p�����[�^
2882     Ovrd 5
2883 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2884 '    Cmp Pos, &B100011
2885 '
2886 '���A����J�n
2887 '
2888 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������
2889 *RecoveryChuckOpen
2890     PActive = P_Curr          '���݈ʒu���擾
2891     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
2892 'PProductOnRoboSet(�˂����{���i�u���ʒu)�́A�`���b�N���
2893     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2894         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2895             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2896                 MRecoveryChuckOpen = 1
2897             EndIf
2898         EndIf
2899     EndIf
2900 'PProductOnRoboGet(�˂����{���i���ʒu)�́A�`���b�N���
2901     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2902         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2903             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2904                 MRecoveryChuckOpen = 1
2905             EndIf
2906         EndIf
2907     EndIf
2908 '
2909     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2910     M_Out(12256) = 0                           '�{�̃`���b�N��OFF
2911     M_Out(12257) = 1                           '�{�̃`���b�N�JON
2912 '
2913     M_20# = 0                                  'KEY���͏�����
2914     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
2915     If MRtn = 1 Then M_Out(12257) = 0          '�{�̃`���b�N�JOFF
2916     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2917 '
2918     fErrorProcess(11,244,284,0)
2919     If M_20# = MNext% Then M_20# = MClear%
2920     If M_20# = MAbout% Then GoTo *RecoveryEnd
2921     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2922     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2923 '
2924     *RecoveryChuckOpenEnd
2925 '
2926 '�w�ʔ��
2927 'PPlateBackSet�`PPlateBackSet_6�̃G���A�ɂ���Ƃ��́A�{�̃`���b�N�J��
2928 '�EPPlateBackSet_6         '�o�H6
2929 '�EPPlateBackSet_5         '�o�H7
2930 '�EPPlateBackSet_4         '�o�H8
2931 '�EPPlateBackSet_3         '�o�H9
2932 '�EPPlateBackSet_2         '�o�H10
2933 '�EPPlateBackSet_1         '�o�H11
2934 '�EPPlateBackSet           '�w�ʔu���ʒu
2935 '��L�V�_�̂w���W�E�x���W�E�y���W��J6�������LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2936     PActive = P_Curr                    '���݈ʒu���擾
2937     JActive = J_Curr                    '���݈ʒu���擾
2938     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2939     If (PActive.X >= -35) And (PActive.X <= -5) Then
2940         If (PActive.Y >= 340) And (PActive.Y <= 510) Then
2941             If (PActive.Z >= 470) And (PActive.Z <= 560) Then
2942                 If (MJ6 >= 160) And (MJ6 <= 200) Then
2943                     M_Out(12256) = 0            '�{�̃`���b�N��OFF
2944                     M_Out(12257) = 1            '�{�̃`���b�N�JON
2945                 Dly 1.0
2946                 EndIf
2947             EndIf
2948         EndIf
2949     EndIf
2950 '
2951 '
2952 '�������@���ځA���ޔ����o���Ȃ����̑Ώ�
2953 '
2954     Ovrd 1
2955 'PProductOnRoboSet(Get)�`PProductOnRoboSet(Get)_2�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_2��
2956 '�EPProductOnRoboSet
2957 '�EPProductOnRoboSet_1
2958 '�EPProductOnRoboSet_2
2959 '�EPProductOnRoboGet
2960 '�EPProductOnRoboGet_1
2961 '�EPProductOnRoboGet_2
2962 '��L�U�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2963     PActive = P_Curr                    '���݈ʒu���擾
2964     JActive = J_Curr                    '���݈ʒu���擾
2965     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2966     If (PActive.X >= -35) And (PActive.X <= 0) Then
2967         If (PActive.Y >= 350) And (PActive.Y <= 420) Then
2968             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
2969                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2970                     Mvs PProductOnRoboSet_1
2971                     Dly 1.0
2972                     Mvs PProductOnRoboSet_2
2973                     Dly 1.0
2974                     Mov PProductOnRoboSet_3
2975                     Dly 1.0
2976                 EndIf
2977             EndIf
2978         EndIf
2979     EndIf
2980 '
2981 'PProductOnRoboSet(Get)_2�`PProductOnRoboSet(Get)_3�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_3��
2982 '�EPProductOnRoboSet_2
2983 '�EPProductOnRoboSet_3
2984 '�EPProductOnRoboGet_2
2985 '�EPProductOnRoboGet_3
2986 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2987     PActive = P_Curr                    '���݈ʒu���擾
2988     JActive = J_Curr                    '���݈ʒu���擾
2989     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2990     If (PActive.X >= -35) And (PActive.X <= 0) Then
2991         If (PActive.Y >= 280) And (PActive.Y <= 390) Then
2992             If (PActive.Z >= 410) And (PActive.Z <= 570) Then
2993                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2994                     Mvs PProductOnRoboSet_3
2995                     Dly 1.0
2996                 EndIf
2997             EndIf
2998         EndIf
2999     EndIf
3000 '
3001     Ovrd 5
3002 '
3003 '���ޔ�
3004     PActive = P_Curr
3005     Pmove = PActive
3006     Pmove.Z = 640           '���ޔ�����ꗥ�̍���
3007     If PActive.X > 550 Then
3008         Pmove.Z =550        '�p���b�g��ɘr��L�΂��Ă���Ƃ���640�܂ŏグ���Ȃ��ׁA��O���u
3009     EndIf
3010     If PActive.Z < Pmove.Z Then
3011         Mvs Pmove
3012     EndIf
3013     Dly 1.0
3014 'J1���ȊO��ޔ��|�W�V�����ֈړ�
3015     JActive = J_Curr
3016     Jmove = JTaihi
3017     Jmove.J1 = JActive.J1        'J1���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
3018     Jmove.J6 = JActive.J6        'J6���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
3019     Mov Jmove
3020     Dly 1.0
3021 'J1���݂̂�ޔ��|�W�V�����ֈړ�
3022     Mov JTaihi
3023     Dly 1.0
3024 '�C�j�V�����|�W�V�����ֈړ�
3025     Mov PInitialPosition
3026     Cmp Off
3027     Ovrd 100
3028 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n
3029     If M_In(11856) = 0 Then                 ' ��~���̂�
3030         fnAutoScreenComment(501)            ' ��ԕ\��[�l�W���ߋ@�����^�]�J�n��] 2022/04/25 �n��
3031         M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
3032         MRet = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
3033         If MRet = 0 Then
3034         Else
3035             M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
3036         EndIf
3037     EndIf
3038     M_Out(12262) = 0            '�ʒu���ߏoOFF
3039     M_Out(12263) = 1            '�ʒu���ߖ�ON
3040     fErrorProcess(11,253,281,0)
3041 *RecoveryEnd
3042     Exit Function
3043 FEnd
3044 '
3045 '
3046 '��fnAutoScreenComment
3047 ''' <summary>
3048 ''' ���C����ʂ̓���󋵕\��
3049 ''' �R�����gD1005�̐ݒ�
3050 ''' </summary>
3051 '''<param name="McommentD1005%">�R�����gID</param>
3052 ''' <remarks>
3053 ''' Date   : 2021/07/07 : M.Hayakawa
3054 ''' </remarks>
3055 Function fnAutoScreenComment(ByVal McommentD1005%)
3056     M_Out16(12576) = McommentD1005%
3057 FEnd
3058 '
3059 '��fnRoboPosChk
3060 ''' <summary>
3061 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
3062 ''' </summary>
3063 '''<param name="MINNumber%">���͔ԍ�</param>
3064 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3065 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3066 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
3067 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
3068 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3069 ''' <remarks>
3070 ''' Date   : 2021/07/07 : M.Hayakawa
3071 ''' </remarks>
3072 Function M% fnRoboPosChk
3073     fnRoboPosChk = 0
3074     MRet = fnStepRead()
3075     '�����ʒu�łȂ��Ɣ��f�����ꍇ
3076     '�E�B���h��ʐ؊���
3077     If MRBTOpeGroupNo > 5 Then
3078         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3079         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3080         Dly 0.2
3081         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3082         Dly 1.5
3083         '
3084         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3085         '
3086         MLoopFlg% = 1
3087         While MLoopFlg% = 1
3088             '
3089             '
3090             MKeyNumber% = fnKEY_WAIT()
3091             Select MKeyNumber%
3092                 Case Is = MAbout%       '��~
3093                     M_20# = MAbout%
3094                     MLoopFlg% = -1
3095                     Break
3096                 Case Is = MNext%        '����
3097                     'MLoopFlg% = -1
3098                     Break
3099                 Case Is = MContinue%    '�p��
3100                     M_20# = MContinue%
3101                     MLoopFlg% = -1
3102                     Break
3103                 Default
3104                     Break
3105             End Select
3106         WEnd
3107     EndIf
3108     '
3109     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
3110         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3111         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
3112         Select MRBTOpeGroupNo
3113             Case Is = 5                          '�������Ȃ�
3114                 Break
3115             Case Is = 10                         '�����ʒu�֖߂�
3116                 'Mov PTEST001
3117                 Break
3118             Case Is = 15                         '�����ʒu�֖߂�
3119                 'Mov PTEST002
3120                 Dly 0.5
3121                 'Mov PTEST001
3122                 Dly 0.5
3123                 Break
3124             Default
3125                 Break
3126         End Select
3127         '
3128         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
3129         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
3130         MRBTOpeGroupNo = 5
3131         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
3132         Dly 1.0
3133         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
3134         fnRoboPosChk = 1                        '�����ʒu������s
3135         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3136     EndIf
3137     Exit Function
3138 FEnd
3139 '
3140 '��frInCheck
3141 ''' <summary>
3142 ''' �Z���T�[IN�`�F�b�N
3143 ''' </summary>
3144 '''<param name="MINNumber%">���͔ԍ�</param>
3145 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3146 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3147 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3148 ''' <remarks>
3149 ''' Date   : 2021/07/07 : M.Hayakawa
3150 ''' </remarks>
3151 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3152     M_Timer(4) = 0
3153     MloopFlg = 0
3154     While MloopFlg = 0
3155         MCrtTime& = M_Timer(4)
3156         If M_In(MINNumber%) = MCMPFLG% Then
3157             MloopFlg = 1
3158             frInCheck = 1
3159         ElseIf MCrtTime& > MTimeCnt& Then
3160             MloopFlg = 1
3161             frInCheck = 0
3162         EndIf
3163     WEnd
3164 FEnd
3165 '-----------------------------------------------
3166 '
3167 '�˂����ߋ@�ʐM�m�F
3168 '
3169 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3170 'fScrewTcomChk = 0�@�F����I��
3171 '          �@ �@ -1 �F�ُ�I��
3172 '-----------------------------------------------
3173 Function M% fScrewTcomChk
3174 *ReCheckScewTcomChk
3175     fScrewTcomChk = 0
3176     '�ʐM�m�F���M
3177     M_Out(MOUT_ScwT_ComChk%) = MOn%
3178     '�ʐM�m�F��M�ҋ@
3179 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3180     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3181     '�ʐM�m�F���M�I��
3182     M_Out(MOUT_ScwT_ComChk%) = MOff%
3183     If MRtn = 0 Then
3184         fScrewTcomChk = -1
3185     EndIf
3186     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
3187  '
3188 FEnd
3189 '
3190 '
3191 '-----------------------------------------------
3192 '
3193 '�˂����ߊJ�n���M
3194 '
3195 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3196 'fScrewTStart = 0�@�F����I��
3197 '           �@�@-1 �F�ُ�I��
3198 '-----------------------------------------------
3199 Function M% fScrewTStart
3200     fScrewTStart = 0
3201     nRet% = 0
3202     '�˂����ߊJ�n�ҋ@����M
3203 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3204     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3205     If MRtn = 0 Then nRet% = -1
3206     If MRtn = 0 Then GoTo *ScrewStartERROR      '�J�n�ł��Ȃ������ꍇ�W�����v
3207     Dly 0.1
3208     '�˂����ߊJ�n��M�𑗐M
3209     M_Out(MOUT_ScwT_ST%) = MOn%
3210     Dly 0.5
3211     'Wait M_In(MTEST_KEY%) = MOn%
3212     '�˂����ߊJ�n���M�I��
3213     M_Out(MOUT_ScwT_ST%) = MOff%
3214     '
3215 *ScrewStartERROR
3216     fScrewTStart = nRet%
3217 FEnd
3218 '
3219 '
3220 '
3221 '-----------------------------------------------
3222 '
3223 '�˂����ߊ�����M
3224 '
3225 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3226 'fScewTFinish = 0�@�F����I��
3227 '          �@ �@-1 �F�ُ�I��
3228 '-----------------------------------------------
3229 Function M% fScewTFinish
3230 *ReCheckScewTFinish
3231     fScewTFinish = 0
3232     '�˂����ߊ����ҋ@����M
3233 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3234     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3235     If MRtn = 0 Then
3236         fScewTFinish = -1
3237     EndIf
3238     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3239     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3240     Dly 0.1
3241     '�˂����ߊ�����M�𑗐M
3242     M_Out(MOUT_ScwT_FinOK%) = MOn%
3243     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3244     '�˂����ߊJ�n���M�I��
3245     M_Out(MOUT_ScwT_FinOK%) = MOff%
3246     'Wait M_In(MTEST_KEY%) = MOn%
3247     '
3248 *ScewTFinish_ErrEnd
3249 FEnd
3250 '
3251 '
3252 '-----------------------------------------------
3253 '
3254 '����xx��~��M
3255 '
3256 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3257 'fScewTCaseStop = 0�@�F����I��
3258 '          �@   �@-1 �F�ُ�I��
3259 '-----------------------------------------------
3260 Function M% fScewTCaseStop(ByVal MCase%())
3261 *ReCheckScewTCaseStop
3262     fScewTCaseStop = 0
3263     '����xx��~����M
3264     Wait M_In(MCase%(1)) = MOn%
3265     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3266     If MRtn = 0 Then
3267         fScewTCaseStop = -1
3268     EndIf
3269     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3270     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3271     Dly 0.1
3272     '����xx��~��M�𑗐M
3273     M_Out(MCase%(2)) = MOn%
3274     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3275     '�˂����ߊJ�n���M�I��
3276     M_Out(MCase%(2)) = MOff%
3277 *ScewTCaseStop_ErrEnd
3278     '
3279 FEnd
3280 '
3281 '��fScrewTighenRoboCheck
3282 '<summary>
3283 '�˂����{�Ď�
3284 '</summary>
3285 '<param name = "MStopNum%"> ��~�ԍ�</param>
3286 '<returns>���� 0:�˂����{�ُ�I�� 1:OK </returns>
3287 '<make>
3288 '2021/12/2 �����V��
3289 '</make>
3290 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3291     fnAutoScreenComment(503)    '��ԕ\��[�˂����{����I���҂�] 2022/04/26 �n��
3292     fScrewTighenRoboCheck = 1
3293     MScrewTighenRoboFlg% = 1    '�t���O�̏�����
3294     MCheck% = 0
3295     While MScrewTighenRoboFlg% = 1
3296         MCheck% = M_In16(11904)
3297         If M_In(MStopNum%) = 1 Then '��~�ʒu�܂ŗ�����
3298             MScrewTighenRoboFlg% = 0 '�֐��𔲂���
3299             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
3300         EndIf
3301         If MCheck% <> 0 Then
3302             fScrewTighenRoboError(MCheck%)
3303             Select M_20#
3304                 Case MAbout%            '��~�������ꂽ�ꍇ
3305                     M_Out(12869) = 1 Dly 1.0
3306                     MScrewTighenRoboFlg% = 0
3307                     fScrewTighenRoboCheck = 0   '�ُ�I��
3308                     Break
3309                 Case MNgProcess%        'NG�������ꂽ�ꍇ
3310                     M_Out(12873) = 1 Dly 1.0
3311                     MScrewTighenRoboFlg% = 0
3312                     fScrewTighenRoboCheck = 0   '�ُ�I��
3313                     Break
3314                 Case MContinue%             '���g���C�������ꂽ�ꍇ
3315                     M_20# = MClear%         'M_20#������
3316                     M_Out(12871) = 1 Dly 1.0
3317                     Break
3318                 Case MNext%                 '���ւ������ꂽ�ꍇ
3319                     M_20# = MClear%         'M_20#������
3320                     M_Out(12874) = 1 Dly 1.0
3321                     Break
3322             End Select
3323             Dly 0.5
3324         EndIf
3325     WEnd
3326 FEnd
3327 '
3328 '��fScrewTighenRoboError
3329 '<summary>
3330 '�˂����{�G���[����
3331 '</summary>
3332 '<param name = "ErrorCode%"> �G���[�ԍ�</param>
3333 '<make>
3334 '2021/12/2 �����V��
3335 '</make>
3336 Function fScrewTighenRoboError(ByVal MErrorCode%)
3337     MErrorScreenCode% = 0
3338     MErrorScreenCode% = MErrorCode% + 300
3339     fErrorProcess(11,MErrorScreenCode%,0,0)
3340 FEnd
3341 '
3342 '��fErrorProcess
3343 '<summary>
3344 '�G���[����
3345 '</summary>
3346 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
3347 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
3348 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
3349 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
3350 '<make>
3351 '2021/11/5 �����V��
3352 '</make>
3353 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3354     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
3355     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
3356     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
3357     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
3358 *RETRY_ERR_PROCESS
3359      M_20# = MClear%     '������
3360 '        '�G���[�����L�q
3361         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3362 '        'GOT KEY���͑҂�
3363         MKeyNumber = fnKEY_WAIT()
3364 '        '
3365         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
3366             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3367             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3368             Break
3369          '
3370         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
3371             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3372             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3373         '
3374         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
3375             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3376             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3377          '
3378         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
3379             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3380             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3381             Break
3382         '
3383         EndIf
3384         '
3385         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3386 FEnd
3387 '
3388 '��fnTorqueCheck
3389 ''' <summary>
3390 ''' �g���N�`�F�b�N����p�̃��C��
3391 ''' </summary>
3392 ''' <remarks>
3393 ''' Date   : 2021/12/21 : H.AJI
3394 ''' </remarks>'
3395 Function M% fnTorqueCheck
3396     '�g���N�`�F�b�N�����M  �����n��~
3397     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3398     '
3399     fnTorqueCheck = 0
3400     Ovrd 20
3401     Mov PInitialPosition              '�����ʒu�ړ�
3402     Ovrd 100
3403     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3404     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3405     Dly 0.2
3406     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3407     '
3408     'M6340  �g���N�`�F�b�N��M
3409     'Dly 5.0
3410     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3411     Dly 1.0
3412     M_Out(12340) = 0
3413     '
3414     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3415     M_Out(12835) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3416    Wait M_In(11843) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3417     M_Out(12835) = 0                         '�˂����{�g���N�`�F�b�N��ʐؑ֊���
3418     '
3419     '
3420     MLoopFlg = 1
3421     While MLoopFlg = 1
3422         '
3423         Mov PInitialPosition              '�����ʒu�ړ�
3424         '
3425         MKeyNumber = fnKEY_WAIT()
3426         Select MKeyNumber
3427             Case Is = 1           '��~
3428                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3429                 Dly 1.0
3430                 M_Out(12343) = 0
3431                 Ovrd 20
3432                 'Mov PTicketRead_1
3433                 M_Out(12840) = 1          '�g���N�`�F�b�N�I��
3434                 Wait M_In(11859) = 1      '�˂����{����̏I��
3435                 M_Out(12840) = 0          '�g���N�`�F�b�N�I��
3436                 Ovrd 100
3437                 M_20# = 1
3438                 MLoopFlg = -1
3439                 Break
3440             Case Is = 2           '����
3441                 Break
3442             Case Is = 3           '�p��
3443                 Break
3444             Case Is = 4           '�g���N�`�F�b�N�J�n
3445                 M_Out(12342) = 1          '�g���N�`�F�b�N�J�n�v����M M6342
3446                 Dly 1.0
3447                 M_Out(12342) = 0
3448                 If M_In(11862) = 1 Then             '�g���N�`�F�b�J�[�m�F
3449                     fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3450                     MRet = fnScrewMTorque()           '�˂����{�p�g���N�`�F�b�N
3451                 EndIf
3452                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3453                 'MRet = fnMoveTorquePosi()
3454                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3455                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3456                 Break
3457             Default
3458                 Break
3459         End Select
3460     WEnd
3461     '
3462     '�g���N�`�F�b�N����~���M
3463     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3464     '
3465     '���{�b�g�̈ʒu�����ɖ߂�
3466     '
3467     '
3468  FEnd
3469  '
3470 '
3471 '
3472 '---------------------------
3473 '
3474 '    ���C����ʂ̕\���A��\���ݒ�
3475 '         �R�����gD1001, D1002, D1003�̐ݒ�
3476 '           MWindReSet = 0     ��ʔ�\��
3477 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3478 '           MWindErrScr = 10    �G���[��� D1001, D1002
3479 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3480 '
3481 '---------------------------
3482 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3483     fnMainScreenOpen = 0
3484     '
3485    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3486         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3487     EndIf
3488     '
3489     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3490         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3491     EndIf
3492     '
3493     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3494         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3495     EndIf
3496     '
3497     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3498     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3499     Dly 0.5
3500     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3501 FEnd
3502 '
3503 '��Main
3504 ''' <summary>
3505 ''' �g���N�`�F�b�N������
3506 ''' </summary>
3507 ''' <remarks>
3508 ''' Date   : 2021/12/21 : H.AJI
3509 ''' </remarks>'
3510 Function M% fnScrewMTorque
3511     fnScrewMTorque = 0
3512     M_Out(12838) = 1                         '�g���N�`�F�b�N�J�n1
3513     Wait M_In(11857) = 1                     '��M����
3514     M_Out(12838) = 0                         '�g���N�`�F�b�N�J�n1
3515     Dly 2.0
3516 FEnd
3517 '
3518 '----------------------------------------------------------------
3519 'fTimeOutJudge
3520 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3521 '����
3522 'Address% = �Ď��A�h���X�ԍ�
3523 'JudgeFlg% = �ΏۃA�h���X�̐���I�����̒l
3524 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3525 '�߂�l = 0 �G���[
3526 '         1 ����I��
3527 '         2 ���g���C
3528 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3529 '�쐬��
3530 '2022/9/20 ����
3531 '----------------------------------------------------------------
3532 '
3533 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3534     fTimeOutJudge = 0
3535     MJudge% = 1
3536     MRtn = 0
3537     M_20# = MClear%
3538     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3539 *TimeOutLoop
3540     If MRtn = 1 Then GoTo *TimeOut
3541         fErrorProcess(11,202,203,0)
3542         If M_20# = MNext% Then GoTo *TimeOutLoop
3543         If M_20# = MContinue% Then MJudge% = 2
3544         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3545 *TimeOut
3546     fTimeOutJudge = MJudge%
3547 '
3548 *JUDGE_ERROR_END
3549 FEnd
3550 '��Main
3551 ''' <summary>
3552 ''' �g������p�̃��C��
3553 ''' </summary>
3554 ''' <remarks>
3555 ''' Date   : 2021/07/07 : M.Hayakawa
3556 ''' </remarks>'
3557 Function Main
3558     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3559     '
3560     If M_Svo=0 Then
3561         Servo On
3562     EndIf
3563     Wait M_Svo=1
3564 '�g���X�^�[�g���t�����v���p���XON
3565     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3566 '�p�g���C�g����
3567     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3568     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3569     '
3570     M_20# = 0                                   'KEY���͏�����
3571     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
3572     MRet% = 0
3573 '�����ʒu�̊m�F�ƈړ�
3574 '
3575 '���A����@���s�E�����s����      2022/04/08 �n�� �쐬
3576     PActive = P_Curr                    '���݈ʒu���擾
3577     MRecoveryPass% = 0
3578     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3579         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3580             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3581                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3582             EndIf
3583         EndIf
3584     EndIf
3585     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3586         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3587             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3588                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3589             EndIf
3590         EndIf
3591     EndIf
3592     If MRecoveryPass% = 0 Then
3593        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3594     EndIf
3595 '
3596 '
3597 '    MRet% = fnRoboPosChk()
3598     If MRet% = 1 Then                           '�����ʒu�̓�����s�����ꍇ
3599         fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  '�E�B���h���
3600         MKeyNumber% = fnKEY_WAIT()
3601         Select MKeyNumber%
3602             Case Is = MAbout%       '��~
3603                 M_20# = MAbout%
3604                 MLoopFlg% = -1
3605                 Break
3606             Case Is = MNext%        '����
3607                 'MLoopFlg = -1
3608                 Break
3609             Case Is = MContinue%    '�p��
3610                 M_20# = MContinue%
3611                 MLoopFlg% = -1
3612                 Break
3613             Default
3614                 Break
3615         End Select
3616     EndIf
3617     '
3618     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3619         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3620 '�g���N�`�F�b�N
3621         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3622             MRet% = fnTorqueCheck()
3623             Break
3624         Else
3625 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
3626 '                MRtn = InspInit()               '�摜��������������
3627 '            EndIf
3628 '
3629             M_20# = MClear%             '������
3630 '�g���J�n
3631             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3632                 fnAssyStart()
3633             Else
3634                 M_20# = MPass%
3635             EndIf
3636 '�g���I�����t����
3637             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3638             Wait M_In(11572) = 1            '���t�擾����
3639             Dly 0.1
3640             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3641 '���t�^�[���j�b�g�ւ�OUT
3642             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3643             fnAutoScreenComment(89)         'AUTO��� �g����������
3644             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3645 'OK/NG�t���O�o��
3646             If M_20# <= 0 Then
3647                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3648             ElseIf M_20# = MPass% Then
3649                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3650             EndIf
3651 'PIAS�ɑg������������
3652             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3653                 If M_20# = MPass% Then
3654                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3655                 Else
3656                     'KEY���͂�NG�̏ꍇ
3657                     If M_20# = MNgProcess% Then
3658                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3659                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3660                         MRet% = fnPiasWrite(MNG%)
3661                        nAssyNgQty = nAssyNgQty + 1
3662                     EndIf
3663                     '
3664                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3665                     If M_20# = MAssyOK% Then
3666                             '-----------------------
3667                             'D732 -> D2600 �R�s�[�v��
3668                             M_Out(12566) = 1
3669 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3670                             M_Out(12566) = 0
3671                             '
3672                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3673                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3674                             '��ԍ��ƍ�(PP�͖��g�p�j
3675 '                            MRet% = fnPCBNumberCheck()
3676                         Else
3677                             MRet% = 1
3678                         EndIf
3679                         '
3680                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3681                             If M_20# <> MAbout% Then
3682                                 '�H������OK��������
3683                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3684                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3685                                 MRet% = fnPiasWrite(MOK%)
3686                                 nAssyOkQty = 0
3687                                 nAssyOkQty = nAssyOkQty + 1
3688                             Else
3689                                 nAssyOkQty = nAssyOkQty + 1
3690                             EndIf
3691                         EndIf
3692                     EndIf
3693 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3694 '                    MRet% = fnPiasWrite(MOK%)
3695                 EndIf
3696             Else
3697                 nAssyOkQty = nAssyOkQty + 1
3698             EndIf
3699             '
3700             '�g���I�����t��������
3701             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3702             '�������A�g��OK���A�g��NG��������
3703 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3704             '
3705 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3706 '                '�摜�����I������
3707 '                MRtn = InspQuit()
3708 '            EndIf
3709         EndIf
3710         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3711     EndIf
3712 '�p�g���C�g����
3713     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3714     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3715 'GOT�\��
3716     fnAutoScreenComment(93)  'AUTO��� �H������
3717 FEnd
3718 End
3719 '
3720 '���܂��Ȃ��R�����g
3721 '��΍폜�����
3722 '
3723 '
3724 '
3725 '
3726 '
3727 '
3728 '
PInspPosition(1)=(+602.00,-150.00,+500.00,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
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
PTemp=(+340.00,+0.03,+579.98,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
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
PActive=(+340.00,+0.03,+579.98,-180.00,+0.00,-180.00,+0.00,+0.00)(7,0)
Pmove=(-18.02,+402.01,+640.00,-179.10,-27.37,+88.86,+0.00,+0.00)(7,1048576)
PInitialPosition=(+340.00,+0.00,+580.00,-180.00,+0.00,+180.00)(7,0)
PMechaGet=(-415.66,-8.74,+299.32,+179.45,-2.22,+176.86)(7,1048577)
PMechaGet_1=(-415.66,-8.74,+409.96,+179.45,-2.22,+176.86)(7,1048577)
PMechaGet_2=(-189.84,-0.01,+629.06,-180.00,+0.00,-179.99)(7,1)
PMechaGet_3=(+0.01,+189.84,+629.07,-180.00,+0.00,+90.00)(7,0)
PMechaGet_4=(+327.50,+0.02,+596.24,-179.99,+0.00,+103.50)(7,0)
PMechaGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaSet1=(+159.80,-334.91,+320.00,+122.02,+86.64,+31.39)(6,0)
PMechaSet1_1=(+159.80,-334.91,+340.00,+122.02,+86.64,+31.39)(6,0)
PMechaSet2=(+160.19,-334.97,+320.18,+122.02,+86.64,+31.36)(6,0)
PMechaSet2_1=(+160.19,-334.97,+339.97,+122.02,+86.64,+31.36)(6,0)
PMechaSet_2=(+162.58,-305.37,+557.38,+179.47,+90.00,+89.47)(6,0)
PMechaSet_3=(+114.45,-288.22,+565.58,+180.00,+0.00,+112.11)(7,0)
PMechaSet_4=(+310.11,-0.04,+565.56,+180.00,+0.00,-179.55)(7,0)
PMechaSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PMechaSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck=(-90.21,+513.03,+577.72,-180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_2=(+66.39,+429.86,+577.75,+180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_3=(-18.78,+286.22,+630.88,+180.00,+0.00,-90.00)(7,0)
PPlateBackCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet=(+477.48,+103.78,+401.13,+179.78,+0.01,+178.57)(7,0)
PPlateBackGet_1=(+477.48,+103.78,+430.00,+179.78,+0.01,+178.57)(7,0)
PPlateBackGet_2=(+477.48,+103.78,+560.00,+179.78,+0.01,+178.57)(7,0)
PPlateBackGet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackSet=(-18.00,+492.33,+543.82,-179.86,-0.33,+89.00)(7,1048576)
PPlateBackSet_1=(-18.00,+446.52,+535.30,-179.37,-13.97,+88.96)(7,1048576)
PPlateBackSet_10=(-18.77,+363.66,+487.69,-179.91,-41.20,+90.53)(7,1048576)
PPlateBackSet_11=(-18.77,+362.28,+493.38,+179.84,-39.34,+90.58)(7,1048576)
PPlateBackSet_12=(-17.62,+286.22,+630.90,-179.82,-0.29,+90.49)(7,1048576)
PPlateBackSet_2=(-18.00,+402.05,+517.82,-179.10,-27.37,+88.87)(7,1048576)
PPlateBackSet_3=(-18.00,+376.88,+500.39,-178.89,-36.13,+88.75)(7,1048576)
PPlateBackSet_4=(-18.83,+358.54,+485.67,-179.10,-42.67,+88.56)(7,1048576)
PPlateBackSet_5=(-18.90,+355.89,+483.22,-179.10,-42.67,+88.56)(7,1048576)
PPlateBackSet_6=(-18.90,+352.51,+487.90,-179.10,-42.67,+88.56)(7,1048576)
PPlateBackSet_7=(-18.83,+378.77,+503.51,+179.34,-35.63,+90.89)(7,1048576)
PPlateBackSet_8=(-18.81,+374.94,+499.62,+179.31,-36.77,+90.90)(7,1048576)
PPlateBackSet_9=(-18.81,+372.36,+496.87,+179.85,-37.92,+90.58)(7,1048576)
PProductOnPltGet=(+479.30,-99.28,+372.11,+179.79,-0.35,+178.62)(7,0)
PProductOnPltGet_1=(+479.30,-99.28,+410.00,+179.79,-0.35,+178.62)(7,0)
PProductOnPltGet_2=(+479.30,-99.28,+500.00,+179.79,-0.35,+178.62)(7,0)
PProductOnPltGet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet=(+478.90,-98.88,+372.11,+179.79,-0.35,+178.62)(7,0)
PProductOnPltSet_1=(+478.90,-98.88,+410.00,+179.79,-0.35,+178.62)(7,0)
PProductOnPltSet_2=(+478.90,-98.88,+500.00,+179.79,-0.35,+178.62)(7,0)
PProductOnPltSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboGet=(-18.40,+403.83,+318.61,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_1=(-18.40,+403.83,+425.20,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_2=(-18.40,+370.97,+425.20,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_3=(-18.40,+300.00,+550.00,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_4=(-18.40,+300.00,+550.00,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboSet=(-18.40,+403.83,+318.61,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboSet_1=(-18.40,+403.83,+425.20,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboSet_2=(-18.40,+370.97,+425.20,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboSet_3=(-18.40,+300.00,+550.00,+75.41,+88.78,+165.60)(6,0)
PProductOnRoboSet_4=(-18.86,+404.69,+360.00,-102.74,+89.08,-12.36)(6,0)
PProductOnRoboSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPushTilt=(-213.31,+564.75,+464.13,+179.96,-0.02,+1.51)(7,0)
PPushTilt_1=(-213.31,+564.75,+480.78,+179.96,-0.02,+1.51)(7,0)
PPushTilt_2=(-213.31,+564.75,+620.00,+179.96,-0.02,+1.51)(7,0)
PPushTilt_3=(+0.02,+340.00,+610.00,-180.00,-0.01,-91.91)(7,0)
PTicketRead=(+602.00,-150.00,+500.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+602.00,-150.00,+550.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
JActive=(+92.75,-1.56,+103.00,-1.14,+51.19,+184.17,+0.00,+0.00)
Jmove=(+92.75,-46.87,+111.64,+0.00,+80.58,+184.17,+0.00,+0.00)
JTaihi=(+0.00,-46.87,+111.64,+0.00,+80.58,+0.00,+0.00,+0.00)
