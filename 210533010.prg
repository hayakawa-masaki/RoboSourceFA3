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
892     Mov PPlateBackSet_13        '�w�ʔu�����
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
918     Mov PPlateBackSet_12        '�ܓ���O���_
919     Cnt 0
920     Ovrd 25
921     Accel 25 , 25
922     Mvs PPlateBackSet_11        '�ܓ��ꍞ�ݑO
923     Mvs PPlateBackSet_10        '�ܓ��ꍞ��1
924     Mvs PPlateBackSet_9         '�ܓ��ꍞ��2
925 '    CmpG 1.0,1.0,1.0,1.0,1.0,1.0,,
926 '    Cmp Pos, &B001000
927     Cnt 1           '�R�����g�A�E�g
928     Mov PPlateBackSet_8         '�o�H1
929     Mov PPlateBackSet_7         '�o�H2
930     Mov PPlateBackSet_6         '�o�H3
931     Mov PPlateBackSet_5         '�o�H4
932     Mov PPlateBackSet_4         '�o�H5
933     Mov PPlateBackSet_3         '�o�H6
934     Mov PPlateBackSet_2         '�o�H7
935     Mov PPlateBackSet_1         '�o�H8
936     Mov PPlateBackSet           '�w�ʔ����ʒu
937 '    Cmp Off
938     Accel 100 , 100
939     Cnt 0
940     Dly 0.1
941 *RE_PLATE_SET
942     M_Out(12256) = 0            '�{�̃`���b�N��OFF
943     M_Out(12257) = 1            '�{�̃`���b�N�JON
944     '
945 '    Wait M_In(11265)            '�{�̃`���b�N�J���o
946     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
947     If MRtn = 1 Then GoTo *CompPlateSet
948     fErrorProcess(11,244,284,0)
949     If M_20# = MNext% Then M_20# = MClear%
950     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
951     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
952     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
953     *CompPlateSet
954 '-----�b�艟��-------------------------------------(22/12/14����)��������
955 *RE_BUCK_PUSH
956     M_20# = MClear%
957     Mov PPlateBackPush_2
958 '
959     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
960     M_Out(12256) = 1            '�{�̃`���b�N��ON
961 '
962     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
963 '
964     If MRtn = 1 Then GoTo *CompBuckPushSetting  '����Ȃ牟�������
965 '
966     fErrorProcess(11,245,287,0) '�[�Z���T�[NG���G���[�\��
967         If M_20# = MNext% Then M_20# = MClear%              '���ւ������ꂽ��l��������
968         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '��~�������ꂽ��G���[�G���h��
969         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END    'NG�������ꂽ��G���[�G���h��
970         If M_20# = MContinue% Then GoTo *RE_BUCK_PUSH       '���g���C�������ꂽ�������x����
971 '
972 *CompBuckPushSetting
973 '
974     Mvs PPlateBackPush_1
975     Ovrd 10
976     Mvs PPlateBackPush
977 '    Dly 0.1     '�N�����v������̂ō폜(221219����)
978 '�w�ʃN�����v��������(12/15)
979     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`��~4)
980     MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
981         If MRtn = 0 Then
982             Mvs PPlateBackPush_1
983             Mov PPlateBackSet_13
984             Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
985         EndIf
986         If MRtn = 0 Then GoTo *ASSY_ERROR_END
987 '�w�ʃN�����v�����܂�(12/15)
988     Ovrd 50                     '20��50�ɕύX(221219����)
989     Mvs PPlateBackPush_1
990 *RE_CHUCK_OPEN
991     M_20# = MClear%
992     M_Out(12256) = 0            '�{�̃`���b�N��OFF
993     M_Out(12257) = 1            '�{�̃`���b�N�JON
994     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
995     If MRtn = 1 Then GoTo *CompChuckOpenForBackPush
996     fErrorProcess(11,244,284,0)
997         If M_20# = MNext% Then M_20# = MClear%              '���ւ������ꂽ��l��������
998         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '��~�������ꂽ��G���[�G���h��
999         If M_20# = MNgProcess% Then GoTo *RE_CHUCK_OPEN    'NG�������ꂽ��G���[�G���h��
1000         If M_20# = MContinue% Then GoTo *RE_CHUCK_OPEN       '���g���C�������ꂽ�������x����
1001 *CompChuckOpenForBackPush
1002 '-----�b�艟��-------------------------------------(22/12/14����)�����܂�
1003 '
1004     ColChk On
1005     Mov PPlateBackSet_13        '�w�ʔu�����
1006     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`��~4)
1007     Ovrd 100
1008     '
1009 ''    ' ���i�����v�����M(�����ʒu�ύX2/27����)
1010 '    M_Out(12787) = 1
1011     '�˂����{���i�N�����v�Œ�҂�
1012 '    Wait M_In(11891) = 1        '�˂����{2��~4��M
1013 MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
1014 If MRtn = 0 Then Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
1015 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1016     '�u���ʒu�摜����
1017 '    Mov PPlateBackCheck_3       '�摜�����ʒu���
1018 '    Mov PPlateBackCheck_2       '�ʉߓ_
1019 '    Mvs PPlateBackCheck         '�m�F�ʒu
1020     '
1021     'PInspPosition(2) = PPlateBackCheck
1022     'MInspGroup(2) = 2
1023     'MRtn = ISInspection(PInspPosition,MInspGroup,2,-1,1)
1024     'If MRtn <> 1 Then
1025     '   '�G���[����
1026     'EndIf
1027     '
1028 ''    ' ���i�����v�����M
1029 '    M_Out(12787) = 1    '�����ʒu�ύX
1030 '    Mov PPlateBackCheck_3       '�摜�����ʒu���
1031     '
1032     '�˂����{�������ݑ҂�
1033     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`����)
1034     '
1035     'DVD���J�����
1036     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1037     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1038     '
1039     Mov PMechaGet_3             '�o�H1
1040     Mov PMechaGet_2             'DVD���J�󂯎������_
1041 '    Wait M_In(11272)            '���i�����@Ready
1042 '    MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '���i�����@Ready
1043 '    If MRtn = 0 Then
1044 '        fErrorProcess()         '�G���[����
1045 '    EndIf
1046 '
1047 '    ' ���i�����v�����M(�����ʒu�ύX)
1048     fnAutoScreenComment(513)    '��ԕ\��[���i�����҂�] 2022/04/26 �n��
1049 '    M_Out(12787) = 1
1050     '    ' ���i���������҂�(�����ύX2/27����)
1051 *RE_FEEDER_READY
1052 '    Wait M_In(11810) = 1
1053 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '�����҂�
1054 If MRtn = 1 Then GoTo *CompFeederReady
1055 '   ' ���i�����v���I��
1056 M_Out(12787) = 0
1057 fErrorProcess(11,289,290,0)                '284��290�ɕύX6/2����
1058 If M_20# = MNext% Then M_20# = MClear%
1059 If M_20# = MAbout% Or M_20# = MNgProcess% Then
1060     Mov PMechaGet_2
1061     Mov PMechaGet_3
1062     Mov PMechaGet_4
1063     Mov PInitialPosition
1064 EndIf
1065 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1066 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1067     ' ���i�����v��
1068 M_Out(12787) = 1
1069 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
1070 *CompFeederReady
1071 '    ' ���i�����v���I��
1072     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1073     M_Out(12787) = 0
1074 '
1075     Mov PMechaGet_1             'DVD���J�󂯎����
1076     '
1077     *RE_MECHA_GET_1
1078     '
1079     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1080     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1081     '
1082 '    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1083     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1084     If MRtn = 1 Then GoTo *CompMechaGet1
1085     Mov PMechaGet_2
1086     Mov PMechaGet_3
1087     Mov PMechaGet_4
1088     fErrorProcess(11,270,284,0)
1089     If M_20# = MNext% Then M_20# = MClear%
1090     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1091     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1092     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1093     Mov PMechaGet_3
1094     Mov PMechaGet_2
1095     Mov PMechaGet_1
1096     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1097     *CompMechaGet1
1098     '
1099     M_Out(12261) = 0            'DVD���J�V�����_�[��OFF
1100     M_Out(12260) = 1            'DVD���J�V�����_�[�oON
1101 '    Wait M_In(11271) = 1        'DVD���J�V�����_�[�o���o
1102     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�o���o
1103     If MRtn = 1 Then GoTo *CompMechaGet2
1104     Mov PMechaGet_2
1105     Mov PMechaGet_3
1106     Mov PMechaGet_4
1107     fErrorProcess(11,271,284,0)
1108     If M_20# = MNext% Then M_20# = MClear%
1109     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1110     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1111     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1112     Mov PMechaGet_3
1113     Mov PMechaGet_2
1114     Mov PMechaGet_1
1115     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1116     *CompMechaGet2
1117     '
1118     Ovrd 25
1119     Mvs PMechaGet               'DVD���J�󂯎��ʒu
1120     Dly 0.1
1121 '
1122     MRtn = 0
1123     MRtn2 = 0
1124     *RE_MECHA_GET_2
1125     If M_20# = MContinue% Then M_20# = MClear%
1126     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1127     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1128     '
1129 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
1130     If MRtn = 1 Then Dly 1.0
1131     If MRtn = 1 Then GoTo *CompMechaGet3
1132     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1133     If M_20# = MNext% Then GoTo *CompMechaGet3
1134     If MRtn = 1 Then GoTo *CompMechaGet3
1135     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1136     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1137     Dly 2.0
1138     Mvs PMechaGet_1
1139     Mov PMechaGet_2
1140     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1141     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1142     Mov PMechaGet_3
1143     Mov PMechaGet_4
1144     fErrorProcess(11,269,284,0)
1145     If M_20# = MNext% Then
1146         M_20# = MClear%
1147         MRtn = 1
1148     EndIf
1149     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1150     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1151     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1152     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1153     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1154     Mov PMechaGet_3
1155     Mov PMechaGet_2
1156     Mov PMechaGet_1
1157     Mvs PMechaGet
1158     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1159     *CompMechaGet3
1160     M_20# = MClear%
1161     '
1162 '    Wait M_In(11267) = 1        'DVD���J���o
1163     If MRtn2 = 1 Then GoTo *CompMechaGet4
1164     MRtn2 = frInCheck(11267,1,MSETTIMEOUT05&)   'DVD���J���o
1165     If MRtn2 = 1 Then GoTo *CompMechaGet4
1166     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1167     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1168     Dly 2.0
1169     Mvs PMechaGet_1
1170     Mov PMechaGet_2
1171     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1172     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1173     Mov PMechaGet_3
1174     Mov PMechaGet_4
1175     fErrorProcess(11,273,284,0)
1176     If M_20# = MNext% Then
1177         M_20# = MClear%
1178         MRtn2 = 1
1179     EndIf
1180     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1181     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1182     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1183     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1184     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1185     Mov PMechaGet_3
1186     Mov PMechaGet_2
1187     Mov PMechaGet_1
1188     Mvs PMechaGet
1189     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1190     *CompMechaGet4
1191     M_20# = MClear%
1192     Dly 0.5
1193     '
1194     Mvs PMechaGet_1             'DVD���J�󂯎����
1195 '    *RE_MECHA_GET_3
1196     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1197     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1198 '    Wait M_In(11270) = 1        'DVD���J�V�����_�[�ߌ��o
1199     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�ߌ��o
1200 '    If MRtn = 1 Then GoTo *CompMechaGet5       '�����ʒu�ύX2/11����
1201 '    fErrorProcess(11,272,284,0)
1202 '    If M_20# = MNext% Then M_20# = MClear%
1203 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1204 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1205 '    If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1206 '    *CompMechaGet5
1207     '
1208     If MRtn = 1 Then Ovrd 100
1209     Mov PMechaGet_2             'DVD���J�󂯎������_
1210 '    ' ���i�����v���I��
1211     M_Out(12787) = 0
1212 '    ' ���i�擾�������M(�p���X)
1213     M_Out(12800) = 1 Dly 0.5
1214     Mov PMechaGet_3             '�o�H1
1215     Mov PMechaGet_4             '�o�H2
1216 '
1217     *RE_MECHA_GET_3
1218     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1219     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1220     If MRtn = 1 Then GoTo *CompMechaGet5
1221     fErrorProcess(11,272,284,0)
1222     If M_20# = MNext% Then M_20# = MClear%
1223     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1224     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1225     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1226     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1227     *CompMechaGet5
1228     '
1229     'DVD���J�����u����֒u��
1230 '    Wait M_In(11920) = 0             'BaseUnit6�����u����t���O�m�F(�����ʒu�ύX2/11����)
1231 '
1232 '   ���u���䂪��]�����m�F
1233     *Loop_CW_CCW_1
1234     If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_1 Else GoTo *Loop_CW_CCW_1
1235     *Next_CW_CCW_1
1236     If M_In(11920) = 0 Then GoTo *OK_FLG_1 Else GoTo *Loop_CW_CCW_1          'BaseUnit6�����u����t���O�m�F
1237     *OK_FLG_1
1238 '
1239     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1240     '
1241     'DVD���J�����u����ɒu����Ă��Ȃ����̊m�F(�ǉ���������10/1����)
1242     MRtn = 1
1243     If M_In(11931) = 1 Then          '��]�����̊m�F(CW����)
1244         If M_In(11928) = 0 Then      'BaseUnit5����DVD���J���u����Ă����ꍇ
1245             M_Out(12912) = 0         '���u����t���O���(��]�\��Ԃɂ��邽��)
1246             Wait M_In(11930) = 1     '���u�����]�҂�
1247             MRtn = 0
1248         EndIf
1249     ElseIf M_In(11930) = 1 Then      '��]�����̊m�F(CCW����)
1250         If M_In(11929) = 0 Then      'BaseUnit5����DVD���J���u����Ă����ꍇ
1251             M_Out(12912) = 0         '���u����t���O���(��]�\��Ԃɂ��邽��)
1252             Wait M_In(11931) = 1     '���u�����]�҂�
1253             MRtn = 0
1254         EndIf
1255     Else
1256         MRtn = 0
1257     EndIf
1258     If MRtn = 0 Then GoTo *Loop_CW_CCW_1
1259     '
1260 *Loop_CW_CCW_S
1261     fnAutoScreenComment(530)    '��ԕ\��[�H���U�̓���I���҂�] 2022/04/26 �n��
1262 'Ver 0.4 �ǉ� -----------------------
1263 '���H����12912 = 1 ���o�͂�����A�ēx �H��6���̓��쒆�Ď����s��
1264     MRtn = 0
1265     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec �H��6�̃t���OON�Ď�
1266     If MRtn = 1 Then M_Out(12912) = 0                  '���u����t���O��� �H��6�D��̂���12912=0���o��
1267     If MRtn = 1 Then Dly 0.7
1268     If MRtn = 1 Then GoTo *Loop_CW_CCW_S        '�H��6�̓��쒆�t���O��ON���Ă�����ēx���[�v
1269     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1270 'Ver 0.4 �����܂� -------------------
1271 '
1272     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1273     Mov PMechaSet_3             'DVD���J���u�����_1
1274 'Dly 5.0   '�f�o�b�O�p
1275 '
1276 *Loop_CW_CCW_2
1277 'Ver 0.4 �ǉ� -----------------------
1278     '���H����12912 = 1 ���o�͂�����A�ēx �H��6���̓��쒆�Ď����s��
1279     MRtn = 0
1280     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec �H��6�̃t���OON�Ď�
1281     If MRtn = 1 Then M_Out(12912) = 0                  '���u����t���O��� �H��6�D��̂���12912=0���o��
1282     If MRtn = 1 Then Dly 0.7
1283     If MRtn = 1 Then GoTo *Loop_CW_CCW_2        '�H��6�̓��쒆�t���O��ON���Ă�����ēx���[�v
1284     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1285 'Ver 0.4 �����܂� -------------------
1286 '
1287     Mov PMechaSet_2             'DVD���J���u�����_2
1288 '
1289 '    *Loop_CW_CCW_2  '���u����̏�Ԃ�������x�m�F����(�R�����g�A�E�g2/27����)
1290 '    If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_2 Else GoTo *Loop_CW_CCW_2
1291 '    *Next_CW_CCW_2
1292 '    If M_In(11920) = 0 Then GoTo *OK_FLG_2 Else GoTo *Loop_CW_CCW_2         'BaseUnit6�����u����t���O�m�F
1293 '    *OK_FLG_2
1294 ''
1295 '    M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1296 '
1297     '
1298     *RE_MECHA_SET_1
1299     If M_20# = MContinue% Then M_20# = MClear%
1300     Ovrd 25
1301     M_Out(12261) = 0            'DVD���J�V�����_�[��OFF
1302     M_Out(12260) = 1            'DVD���J�V�����_�[�oON
1303 '    Wait M_In(11271) = 1        'DVD���J�V�����_�[�o���o
1304     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�o���o
1305     If MRtn = 1 Then GoTo *CompMechaSet1
1306     Mov PMechaSet_3
1307     Mov PMechaGet_4
1308     fErrorProcess(11,271,284,0)
1309     If M_20# = MNext% Then M_20# = MClear%
1310     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1311     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1312     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1313     Mov PMechaSet_3
1314     Mov PMechaSet_2
1315     Ovrd 100
1316     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_1
1317     *CompMechaSet1
1318     '
1319     *RE_MECHA_SET_12
1320     Fine 0.05 , P
1321 '    Wait M_In(11920) = 0        'BaseUnit6�����u����t���O�m�F(�R�����g�A�E�g2/27����)
1322 '    M_Out(12912) = 1            '���u����t���O����
1323     If M_In(11931) = 1 Then     '��]�����̊m�F(CW����)(�ǉ������܂�10/1����)
1324         Mov PMechaSet1_1        'DVD���J���u�����1
1325         Ovrd 10
1326         Mvs PMechaSet1          'DVD���J���u���ʒu1
1327         Dly 0.1
1328         M_Out(12258) = 0        'DVD���J�`���b�N��OFF
1329         M_Out(12259) = 1        'DVD���J�`���b�N�JON
1330 '        Wait M_In(11268) = 1    'DVD���J�`���b�N�J���o
1331         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1332         Mvs PMechaSet1_1        'DVD���J���u�����1
1333     ElseIf M_In(11930) = 1 Then '��]�����̊m�F(CCW����)(�ǉ���������10/1����)
1334         Mov PMechaSet2_1        'DVD���J���u�����
1335         Ovrd 10
1336         Mvs PMechaSet2          'DVD���J���u���ʒu2
1337         Dly 0.1
1338         M_Out(12258) = 0        'DVD���J�`���b�N��OFF
1339         M_Out(12259) = 1        'DVD���J�`���b�N�JON
1340 '        Wait M_In(11268) = 1    'DVD���J�`���b�N�J���o
1341         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1342         Mvs PMechaSet2_1        'DVD���J���u�����2
1343     'Else
1344         '�G���[��(���u���䂪����Ȉʒu�ɖ���)
1345     EndIf                       '�ǉ������܂�10/1����
1346     Fine 0 , P
1347     '
1348     If MRtn = 1 Then GoTo *CompMechaSet2
1349     fErrorProcess(11,270,284,0)
1350     If M_20# = MNext% Then M_20# = MClear%
1351     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1352     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1353     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_12
1354     *CompMechaSet2
1355     '
1356     Ovrd 100
1357     *RE_MECHA_SET_2
1358     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1359     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1360 '    Wait M_In(11270) = 1        'DVD���J�V�����_�[�ߌ��o
1361     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�ߌ��o
1362     If MRtn = 1 Then GoTo *CompMechaSet3
1363     fErrorProcess(11,272,284,0)
1364     If M_20# = MNext% Then M_20# = MClear%
1365     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1366     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1367     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_2
1368     *CompMechaSet3
1369     '
1370     Mov PMechaSet_2             'DVD���J���u�����_2
1371     M_Out(12912) = 0                  '���u����t���O���(�ǉ�10/1����)
1372     '
1373     '�˂����{2�̐��i�����
1374     Mov PProductOnRoboGet_4     '�o�H3����4��
1375     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1376     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1377 '    Wait M_In(11876) = 1        '�˂����{2�����ҋ@����M
1378 MRtn = fScrewTighenRoboCheck(11876)    '��~��Ԃ���M����
1379 If MRtn = 0 Then Mov PInitialPosition   '"�C�j�V�����ɖ߂铮��"
1380 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1381 '
1382     *RE_ROBO_GET_1
1383 '
1384     M_Out(12259) = 0            'DVD���J�`���b�N�JOFF
1385     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1386     If M_20# = MContinue% Then Dly 0.5
1387 '
1388 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
1389     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1390     If MRtn = 1 Then GoTo *CompRoboGet1
1391     fErrorProcess(11,269,284,0)
1392     If M_20# = MNext% Then M_20# = MClear%
1393     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1394     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1395     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1396     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1397     *CompRoboGet1
1398     '
1399 '    Ovrd 50
1400     Mov PProductOnRoboGet_3     '�˂����{���i�������_2����3��
1401 '    Ovrd 20
1402     Mvs PProductOnRoboGet_2     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����2��
1403     Ovrd 20
1404     Mvs PProductOnRoboGet_1     '�˂����{���i�����
1405     Ovrd 10
1406     Mvs PProductOnRoboGet       '�˂����{���i���ʒu
1407     Dly 0.2
1408 '
1409     *RE_ROBO_GET_2
1410 '
1411     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1412     M_Out(12256) = 1            '�{�̃`���b�N��ON
1413 '
1414 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
1415     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
1416     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
1417     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1418     M_Out(12257) = 1            '�{�̃`���b�N�JON
1419     Dly 2.0
1420     Mvs PProductOnRoboGet_1
1421     Mvs PProductOnRoboGet_2
1422     Mov PProductOnRoboGet_3
1423     Mov PProductOnRoboGet_4
1424     Mov PInitialPosition
1425     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1426     M_Out(12256) = 1            '�{�̃`���b�N��ON
1427     Dly 1.0
1428     fErrorProcess(11,245,284,0)
1429     If M_20# = MNext% Then MRtn = 1
1430     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1431     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1432     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1433     M_Out(12257) = 1            '�{�̃`���b�N�JON
1434     Dly 2.0
1435     Mov PProductOnRoboGet_4
1436     Mov PProductOnRoboGet_3
1437     Mov PProductOnRoboGet_2
1438     Mvs PProductOnRoboGet_1
1439     Mvs PProductOnRoboGet
1440     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
1441     *CompRoboGet2
1442     M_20# = MClear%
1443     '
1444     Dly 0.2
1445     Mvs PProductOnRoboGet_1     '�˂����{���i�����
1446     Ovrd 50
1447     Mvs PProductOnRoboGet_2     '�˂����{���i�������_(9/27�b��R�����g�A�E�g)12/15�R�����g����
1448     Mvs PProductOnRoboGet_3     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����3��
1449     Ovrd 100
1450     Mov PProductOnRoboGet_4     '�o�H3����4��
1451     Cnt 1 , 10 , 10
1452 '
1453     M_Out(12868) = 1 Dly 0.3    '�˂����{2���슮���𑗐M
1454     *RE_ROBO_GET_3
1455     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1456     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1457 '    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1458     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1459     If MRtn = 1 Then GoTo *CompRoboGet3
1460     fErrorProcess(11,270,284,0)
1461     If M_20# = MNext% Then M_20# = MClear%
1462     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1463     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1464     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1465     *CompRoboGet3
1466     '
1467     '�p���b�g�֐��i��u��
1468     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1469     Cnt 1 , 10
1470     Mov PProductOnPltSet_1      '�{�̒u���ʒu���
1471     Cnt 0
1472     Ovrd 10
1473     Mvs PProductOnPltSet        '�{�̒u���ʒu
1474     Dly 0.5
1475 '
1476     *RE_PLT_SET
1477 '
1478     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1479     M_Out(12257) = 1            '�{�̃`���b�N�JON
1480 '
1481     Wait M_In(11265) = 1        '�{�̃`���b�N�J���o
1482 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1483     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1484     If MRtn = 1 Then GoTo *CompPltSet
1485     fErrorProcess(11,244,284,0)
1486     If M_20# = MNext% Then M_20# = MClear%
1487     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1488         Mvs PProductOnPltSet_1
1489         Mov PProductOnPltSet_2
1490         Mov PInitialPosition
1491     EndIf
1492     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1493     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1494     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1495     *CompPltSet
1496 '
1497     Mvs PProductOnPltSet_1      '�{�̒u���ʒu���
1498     Ovrd 100
1499     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1500 '    Mov PInitialPosition        '�C�j�V�����|�W�V����
1501     MRtn = FnCtlValue2(2)          '�g���n�j�{�P  2022/04/28 �n��
1502     Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
1503     MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1504     '
1505     '�`�P�b�gID��������
1506     M_20# = MAssyOK%
1507     *ASSY_ERROR_END
1508     *AssyEnd
1509     *fnAssyStart_FEndPosi
1510 FEnd
1511 '
1512 '��fnPiasCheck
1513 ''' <summary>
1514 ''' PIAS�`�P�b�g�Ǎ���
1515 ''' </summary>
1516 ''' <returns>   0 : NG
1517 '''             1 : OK(�Ǎ��݊���)
1518 ''' </returns>
1519 ''' <remarks>
1520 ''' Date   : 2021/07/07 : M.Hayakawa
1521 ''' </remarks>'
1522 Function M% fnPiasCheck
1523     fnPiasCheck = 0
1524     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1525     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1526 '
1527 *RETRY_PIAS
1528     M_20# = MClear%
1529     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1530     '
1531     '�yID�`�P�b�g�ǂݍ��݁z
1532     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1533     MInspGroup%(1) = 1              '����G�ԍ�
1534     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1535 '
1536     '�G���[�̏ꍇ
1537     If MRtn <> 1 Then
1538         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1539         If MRtn <> 1 Then
1540             'D720 -> D1300 �R�s�[�v��
1541             M_Out(12565) = 1
1542             Dly 0.5
1543             M_Out(12565) = 0
1544             '�G���[�����L�q
1545             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1546             'GOT KEY���͑҂�
1547             MKeyNumber = fnKEY_WAIT()
1548             '
1549             Select MKeyNumber
1550                 Case MNext%         '���ւ�I�������ꍇ
1551                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1552                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1553                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1554                     Break
1555                 Case MAbout%        '��~��I�������ꍇ
1556                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1557                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1558                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1559                     Break
1560                 Case MNgProcess%    'NG��I�������ꍇ
1561                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1562                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1563                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1564                     Break
1565                 Case MContinue%     '�p����I�������ꍇ
1566                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1567                     M_20# = MContinue%
1568                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1569                     Break
1570             End Select
1571         EndIf
1572     EndIf
1573 '----------D720 -> D1300 �R�s�[�v��----------
1574     M_Out(12565) = 1
1575     Dly 0.5
1576     M_Out(12565) = 0
1577 '----------�ʐM�m�F������----------
1578     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1579     MRtn = 0                ' ������
1580     M_20# = MClear%         ' ������
1581     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1582     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1583     If MRtn <> 1 Then
1584         If M_20# = MContinue% Then
1585             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1586         Else
1587             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1588         EndIf
1589     EndIf
1590 '----------�H�������m�F----------
1591     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1592     MRtn = 0                ' ������
1593     M_20# = MClear%         ' ������
1594     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1595     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1596     If MRtn <> 1 Then
1597         If M_20# = MContinue% Then
1598             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1599         Else
1600             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1601         EndIf
1602     EndIf
1603     '
1604     fnPiasCheck = 1
1605     *fnPiasCheck_End
1606 FEnd
1607 '
1608 '��fnPCComuCheck
1609 ''' <summary>
1610 ''' PC-PLC�ʐM�`�F�b�N
1611 ''' </summary>
1612 ''' <returns>   0 : NG
1613 '''             1 : OK(�Ǎ��݊���)
1614 ''' </returns>
1615 ''' <remarks>
1616 ''' Date   : 2021/07/07 : M.Hayakawa
1617 ''' </remarks>'
1618 Function M% fnPCComuCheck
1619     fnPCComuCheck = 0
1620     MJudge% = 0                                  '������
1621     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1622     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1623     '
1624     For MStaNo = 0 To 5
1625         '
1626         If M_In(MIN_PIAS_ComOK%) = 1 Then
1627             'PC�ʐMOK(M400)
1628             MJudge% = MOK%
1629             MStaNo = 5
1630             Break
1631         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1632             'toRBT_�ʐM�m�Ftime out
1633             MJudge% = MNG%
1634             MCommentD1001 = 15
1635             MCommentD1002 = 21
1636             MStaNo = 5
1637             Break
1638         Else
1639             'toRBT_�ʐM�m�Ftime out
1640             MJudge% = MNG%
1641             MCommentD1001 = 14
1642             MCommentD1002 = 21
1643             Break
1644         EndIf
1645     Next MStaNo
1646     '
1647     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1648     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1649     '
1650     '�G���[���
1651     If MJudge% <> MOK% Then
1652         M_20# = MClear%     '������
1653         '�G���[�����L�q
1654         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1655         'GOT KEY���͑҂�
1656         MKeyNumber = fnKEY_WAIT()
1657         '
1658         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1659             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1660             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1661             Break
1662         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1663             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1664             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1665             Break
1666         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1667             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1668             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1669             Break
1670         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1671             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1672             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1673             Break
1674         EndIf
1675     Else
1676         'OK�̏ꍇ
1677         fnPCComuCheck = 1
1678     EndIf
1679 FEnd
1680 '
1681 '��fnProcessCheck
1682 ''' <summary>
1683 ''' �H�������m�F
1684 ''' </summary>
1685 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1686 '''             -1�F�O�H������NG  -2�F���H����������
1687 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1688 '''             -5�F���������G���[
1689 ''' </returns>
1690 ''' <remarks>
1691 ''' Date   : 2021/07/07 : M.Hayakawa
1692 ''' </remarks>'
1693 Function M% fnProcessCheck
1694     fnProcessCheck = 0
1695     MJudge% = MNG%      '��UNG���������Ƃ���
1696 '----------�H�������m�F----------
1697     MCommentD1001 = 0   '�R�����g������
1698     For MStaNo = 0 To 5
1699         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1700         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1701         '
1702         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1703             MJudge% = MOK%
1704             fnAutoScreenComment(85)     ' AUTO���
1705             MStaNo = 5
1706             Break
1707         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1708             MFlgLoop% = 0
1709             MJudge% = MNG%
1710             MCommentD1001 = 27
1711             MCommentD1002 = 22
1712             fnAutoScreenComment(94)     ' AUTO���
1713             fnProcessCheck = -2         ' NG��-2��Ԃ�
1714             MStaNo = 5
1715             Break
1716         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1717            MJudge% = MNG%
1718             MCommentD1001 = 31
1719             MCommentD1002 = 22
1720             fnAutoScreenComment(83)     ' AUTO���
1721             fnProcessCheck = -3         ' NG��-3��Ԃ�
1722             MStaNo = 5
1723             Break
1724         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1725             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1726             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1727             MJudge% = MNG%
1728             MCommentD1001 = 32
1729             MCommentD1002 = 22
1730             fnAutoScreenComment(84)     ' AUTO���
1731             fnProcessCheck = -1         ' NG��-1��Ԃ�
1732             Dly 1.0
1733             '�H�������m�FOFF
1734             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1735             Dly 1.0
1736            'MStaNo = 5
1737             Break
1738         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1739             MFlgLoop% = 0
1740             MJudge% = MNG%
1741             MCommentD1001 = 29
1742             MCommentD1002 = 22
1743             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1744             fnProcessCheck = -5         ' NG��-5��Ԃ�
1745             MStaNo = 5
1746             Break
1747         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1748             MJudge% = MNG%
1749             If MCommentD1001 = 32 Then
1750                 '�������Ȃ�
1751             Else
1752                 MCommentD1001 = 26
1753             EndIf
1754             MCommentD1002 = 22
1755             fnProcessCheck = -4         ' NG��-4��Ԃ�
1756             MStaNo = 5
1757             Break
1758         Else
1759             MJudge% = MNG%
1760             MCommentD1001 = 28
1761             MCommentD1002 = 22
1762         EndIf
1763     Next MStaNo
1764     '�H�������m�FOFF
1765     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1766     '�ʉߗ���NG �H�������̏ꍇ
1767     If MJudge% = MPass% Then
1768         M_20# = MPass%
1769     EndIf
1770     '
1771     '�G���[���
1772     If MJudge% <> MOK% Then
1773         M_20# = MClear%     '������
1774         '�G���[�����L�q
1775         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1776         'GOT KEY���͑҂�
1777         MKeyNumber = fnKEY_WAIT()
1778         '
1779         Select MKeyNumber
1780             Case MAbout%        '��~��I�������ꍇ
1781                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1782                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1783                 Break
1784             Case MNext%         '���ւ�I�������ꍇ
1785                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1786                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1787                 Break
1788             Case MContinue%     '�p����I�������ꍇ
1789                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1790                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1791                 Break
1792             Case MNgProcess%    'NG��I�������ꍇ
1793                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1794                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1795                 Break
1796         End Select
1797     Else
1798         fnProcessCheck = 1  ' OK��1��Ԃ�
1799     EndIf
1800 FEnd
1801 '
1802 '��fnPiasWrite
1803 ''' <summary>
1804 ''' Pias �g�����ʏ����ݗv��
1805 ''' </summary>
1806 '''<param name="MFlg%">
1807 '''                 MOK%(1) = �H��������OK��������
1808 '''                 MNG%(0) = �H��������NG��������
1809 '''</param>
1810 '''<returns></returns>
1811 ''' <remarks>
1812 ''' Date   : 2021/07/07 : M.Hayakawa
1813 ''' </remarks>'
1814 Function M% fnPiasWrite(ByVal MFlg%)
1815       fnPiasWrite = 0
1816 *RETRY_PIASWRITE
1817     '
1818     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1819    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1820     If MFlg% = MOK% Then
1821         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1822     Else
1823         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1824     EndIf
1825     Dly 0.1                  '�O�̂���
1826     '
1827     'Pias�֏����݊J�n M305 -> ON
1828     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1829     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1830     '
1831     MJudge% = MNG%
1832     '
1833     For MStaNo = 0 To 5
1834         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1835             MJudge% = MOK%
1836             'MRet = fnAutoScreenComment(85)  'AUTO���
1837             MStaNo = 5
1838             Break
1839         '
1840         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1841             MJudge% = MNG%
1842             'MRet = fnAutoScreenComment(85)  'AUTO���
1843            MCommentD1001 = 34
1844            MCommentD1002 = 25
1845             MStaNo = 5
1846             Break
1847         '
1848         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1849             MJudge% = MNG%
1850             'MRet = fnAutoScreenComment(85)  'AUTO���
1851            MCommentD1001 = 35
1852            MCommentD1002 = 25
1853             MStaNo = 5
1854             Break
1855         '
1856         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1857             MJudge% = MNG%
1858             'MRet = fnAutoScreenComment(85)  'AUTO���
1859            MCommentD1001 = 36
1860            MCommentD1002 = 25
1861             MStaNo = 5
1862             Break
1863         '
1864         Else
1865             MJudge% = MNG%
1866            MCommentD1001 = 42
1867            MCommentD1002 = 25
1868         '
1869         EndIf
1870         '
1871     Next MStaNo
1872     '
1873     'Pias�֏����݊J�n M305 -> OfF
1874     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1875     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1876     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1877     '
1878     '
1879     '�ʉߗ���NG �H�������̏ꍇ
1880     If MJudge% = MPass% Then
1881         M_20# = MPass%
1882     EndIf
1883     '
1884    M_20# = MClear%     '������
1885     '
1886     '�G���[���
1887     If MJudge% < MOK% Then
1888     '
1889 '�c���Ă���������ł͎g�p���Ȃ����x��
1890 *RETRY_ERR_WRITE
1891         M_20# = MClear%     '������
1892         '�G���[�����L�q
1893         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1894         'GOT KEY���͑҂�
1895         MKeyNumber = fnKEY_WAIT()
1896         '
1897         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1898             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1899            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1900             Break
1901         '
1902         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1903             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1904             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1905         '
1906         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1907             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1908             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1909         '
1910         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1911             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1912            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1913             Break
1914         '
1915         EndIf
1916         '
1917         If M_20# = MClear% Then *RETRY_ERR_WRITE
1918         '
1919     EndIf
1920     '
1921     If M_20# = MContinue% Then *RETRY_PIASWRITE
1922     '
1923     fnPiasWrite = 1
1924     '
1925 FEnd
1926 '
1927 '��fnPCBNumberCheck
1928 ''' <summary>
1929 ''' Pias ��ԍ��ƍ��v��
1930 ''' </summary>
1931 '''<param name="%"></param>
1932 '''<param name="%"></param>
1933 '''<returns></returns>
1934 ''' <remarks>
1935 ''' Date   : 2021/07/07 : M.Hayakawa
1936 ''' </remarks>'
1937 Function M% fnPCBNumberCheck
1938       fnPCBNumberCheck = 0
1939     '
1940 *RETRY_PCBCHECK
1941     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1942     'Pias�֊�ƍ��J�n M310 -> ON
1943     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1944     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1945     '
1946     MJudge% = MNG%
1947     '
1948     For MStaNo = 0 To 5
1949         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1950             MJudge% = MOK%
1951             fnAutoScreenComment(96)  'AUTO���
1952             MStaNo = 5
1953             Break
1954         '
1955         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1956             MJudge% = MNG%
1957             fnAutoScreenComment(97)  'AUTO���
1958             MCommentD1001 = 37
1959             MCommentD1002 = 25
1960             MStaNo = 5
1961             Break
1962         '
1963         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1964             MJudge% = MNG%
1965             fnAutoScreenComment(98)  'AUTO���
1966             MCommentD1001 = 38
1967             MCommentD1002 = 25
1968             MStaNo = 5
1969             Break
1970         '
1971         ElseIf M_In(11580) = 1 Then                         'time out
1972             MJudge% = MNG%
1973             fnAutoScreenComment(99)  'AUTO���
1974             MCommentD1001 = 39
1975             MCommentD1002 = 25
1976             MStaNo = 5
1977             Break
1978         '
1979         Else
1980             MJudge% = MNG%
1981            MCommentD1001 = 41
1982            MCommentD1002 = 25
1983         '
1984         EndIf
1985         '
1986     Next MStaNo
1987     '
1988     'Pias�֊�ƍ��J�n M310 -> OfF
1989     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1990     '
1991     '
1992     '�ʉߗ���NG �H�������̏ꍇ
1993     If MJudge% = MPass% Then
1994         M_20# = MPass%
1995     EndIf
1996     '
1997    M_20# = MClear%     '������
1998     '
1999     '�G���[���
2000     If MJudge% < MOK% Then
2001     '
2002 '�c���Ă���������ł͎g�p���Ȃ����x��
2003 *RETRY_ERR_PCBNUMBER
2004         M_20# = MClear%     '������
2005         '�G���[�����L�q
2006         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
2007         'GOT KEY���͑҂�
2008         MKeyNumber = fnKEY_WAIT()
2009         '
2010         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2011             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2012             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2013             Break
2014         '
2015         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2016             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2017             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2018         '
2019         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2020             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2021             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2022         '
2023         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2024             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2025             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2026             Break
2027         '
2028         EndIf
2029         '
2030         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
2031         '
2032     EndIf
2033     '
2034     If M_20# = MContinue% Then *RETRY_PCBCHECK
2035 FEnd
2036 '
2037 '��ScrewTight_S2
2038 ''' <summary>
2039 ''' �˂����߂��s��
2040 ''' </summary>
2041 '''<param name="PScrewPos()">
2042 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
2043 '''             PScrewPos(2)    �F�˂����߉��_
2044 '''             PScrewPos(10)   �F�˂����ߏI������
2045 '''</param>
2046 '''<returns>����
2047 '''         0=�ُ�I���A1=����I��
2048 '''</returns>
2049 ''' <remarks>
2050 ''' Date   : 2021/07/07 : M.Hayakawa
2051 ''' </remarks>'
2052 Function M% ScrewTight_S2(ByVal PScrewPosition())   '�l�W���ߌʐݒ�
2053     ScrewTight_S2 = 0
2054     MOKNGFlg = 0
2055     Ovrd 100
2056     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
2057     ' �b��
2058     Ovrd 5
2059     Mvs PScrewPosition(10),-10    ' �p���b�g��˂�����S�@�̏��ֈړ�
2060 '    Ovrd MOvrdA
2061     '�b��}�X�N
2062 '    M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
2063 '    Dly 0.1
2064 '    M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
2065 '    'Spd 8.3 '�O�����C�h100  �������C�h60   '���C�h100-40�@100%�FSpd�@15�@'�˂����ߑ��x�ݒ�
2066 '    Spd MSpdA               '�l�W���ߎ�Spd�ʐݒ�
2067     ' �b��ړ��̂�
2068     Mvs PScrewPosition(10)
2069 '    '
2070 '    Dly 0.1
2071 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
2072 '    Wait M_In(11584)=1          '����/�G���[���o
2073 '    Dly 0.1
2074 '    Spd M_NSpd
2075 '    '
2076 '    If M_In(X28_Driver)=1 Then  '�˂��g�[�^���G���[���o��
2077 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
2078 '        Dly 0.1
2079 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
2080 '        Dly 0.1
2081 '        M_Out(Y63_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
2082 '        Dly 0.1
2083 '        M_Out(Y65_Driver)=0     '�v���O���������@F1
2084 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
2085 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
2086 '        MOKNGFlg = -1
2087 '        ScrewTight_S2 = 0
2088 '    Else
2089 '        Wait M_In(X29_Driver)=1 ' ���튮����
2090 '        Dly 0.1
2091 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
2092 '        Dly 0.1
2093 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O����
2094 '        Dly 0.1
2095 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
2096 '        Dly 0.1
2097 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
2098 '        ScrewTight_S2 = 1
2099 '    EndIf
2100 ' �b��
2101     Ovrd 10
2102     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
2103     Ovrd 100
2104 FEnd
2105 '
2106 '��ScrewGet_S3
2107 ''' <summary>
2108 ''' �˂������@����˂��𓾂�
2109 ''' </summary>
2110 '''<param name="%"></param>
2111 '''         PScrewPos(1)    �F�˂�������̂˂����
2112 '''         PScrewPos(2)    �F�˂���������_
2113 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2114 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
2115 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
2116 '''<returns>����
2117 '''         0=�ُ�I���A1=����I���A-1=M�l�W�Z���T�[NG�A-2=M�l�W�Z���T�[ON�A-3=�z���G���[
2118 '''</returns>
2119 ''' <remarks>
2120 ''' Date   : 2021/07/07 : M.Hayakawa
2121 ''' </remarks>'
2122 Function M% ScrewGet_S3(ByVal PScrewPosition())
2123     ScrewGet_S3 = 0
2124     MMScrewJudge% = 0
2125     '�˂������평������G���[�`�F�b�N
2126 ' ���b��폜
2127 '    Wait M_In(X34_ScrewReady1)=1 '�˂�������S��Ready�ɂȂ�܂ő҂@���@���b���҂���Ready�ɂȂ�Ȃ���Δ�����v���O�������K�v�H
2128 '    Ovrd 100
2129 '    If M_In(X33_SS2)=0 Then  'M�˂����o�Z���T��OFF�i�̏�j���Ă����ꍇ
2130 '        Ovrd 30
2131 '        Mvs,-80             '���̏ꏊ����80mm���ֈړ�
2132 '        Mov PInitPos19049   '19049�����ʒu�ֈړ�
2133 '        M_Out(Y6A_VV1)=0    '�˂��z�� Off
2134 '        'NG�Ƃ��Ă����̊֐����甲����
2135 '        ScrewGet_S3 = -1
2136 '        MMScrewJudge% = 1
2137 '        MCommentD1001 = 61
2138 '    EndIf
2139 '    If ScrewGet_S3 = 0 Then
2140 '        'S�^�C�g�p�˂������@��M�˂����������Ă��Ȃ����Ď�
2141 '        MMScrewJudge% = 0 'MMScrewJudge������������
2142 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
2143 '        If MRtn = 0 Then
2144 '            Ovrd 30
2145 '            Mvs,-80            '���̏ꏊ����50mm���ֈړ�
2146 '            Mov PInitPos19049  '19049�����ʒu�ֈړ�
2147 '            MMScrewJudge% = 2
2148 '            MRtn = All_CLamp_Release()'�S�ẴN�����v�����֕���
2149 '            MCnt% = 2   '2��ݒ�
2150 '            MCommentD1001 = 62
2151 '        EndIf
2152 '        If MMScrewJudge% = 2 Then
2153 '            ScrewGet_S3 = -2
2154 '        EndIf
2155 '    EndIf
2156 '    'M�l�W���肪ON�̏ꍇ NG�Ƃ��Ċ֐��𔲂���
2157 '    If MMScrewJudge% = 2 Then
2158 '        ScrewGet_S3 = -2
2159 '    EndIf
2160     'S�l�W�p�˂����Y��M�l�W�����m�F�p�����܂�
2161     Ovrd 100
2162     Spd M_NSpd
2163     If MMScrewJudge% = 0 Then
2164         ScrewGet_S3 = 0
2165         M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
2166         MScrewCnt% = 0
2167         MFinCnt% = 2
2168 '        For MCnt% = 0 To MFinCnt%
2169             Mov PScrewPosition(2)        ' �˂������@���_
2170             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
2171             Ovrd 80
2172             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2173             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
2174             Mvs PScrewPosition(10), 1.2
2175             M_Out(Y6A_VV1)=1        ' �˂��z���@ON
2176             '�r�b�g��]
2177             M_Out(Y60_Driver)=1
2178             Dly 0.2
2179             '
2180             Ovrd 100
2181             JOvrd M_NJovrd
2182             Spd M_NSpd
2183             '�l�W�z���m�F�ʒu�ړ�
2184             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
2185             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
2186             '�r�b�g��]��~
2187             'M_Out(Y60_Driver)=0
2188             '
2189             '1�b�ԃl�W�z���m�F
2190 ' �ȉ��b��폜
2191 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2192 '            'MRtn = 0'�����G���[
2193 '            '�z���G���[�̏ꍇ
2194 '            '�l�W���˂����Y�ɖ߂�
2195 '            If MRtn = 0 Then
2196 '                Ovrd 30
2197 '                '�r�b�g��]��~
2198 '                M_Out(Y60_Driver)=0
2199 '                '�l�W�����@���
2200 '                Mvs PScrewPos(1)
2201 '                '�X�ɏ��
2202 '                Mov PScrewPos(1), -75
2203 '                '�l�W�̂Ĉʒu
2204 '                Mov PScrewFeedS021
2205 '                '�z��OFF
2206 '                M_Out(Y6A_VV1)=0 '�˂��z���@OFF
2207 '                Dly 0.2
2208 '                '�j��ON
2209 '                M_Out(Y6B_VB1)=1 '�^��j��ON
2210 '                '�r�b�g��]
2211 '                M_Out(Y61_Driver)=1
2212 '                Dly 0.5
2213 '                '
2214 '                Ovrd 100
2215 '                JOvrd M_NJovrd
2216 '                Spd M_NSpd
2217 '                '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
2218 '                Mov PScrewFeedS021, 10
2219 '                Mov PScrewFeedS021
2220 '                Dly 0.1
2221 '                Mov PScrewFeedS021, 10
2222 '                Mov PScrewFeedS021
2223 '                '
2224 '                '�l�W�����҂�
2225 '                '�r�b�g��]��~
2226 '                M_Out(Y61_Driver)=0
2227 '                Dly 0.1
2228 '                '�j��OFF
2229 '                M_Out(Y6B_VB1)=0 '�^��j��OFF
2230 '                '
2231 '                '
2232 '                '�˂��������Ƃ��āA�ړ��X�ɏ��
2233 '                Mov PScrewPos(1), -75
2234 '                Ovrd 100
2235 '                Spd M_NSpd
2236 '                '�l�W�����@���
2237 '                Mvs PScrewPos(1)
2238 '                '
2239 '                ScrewGet_S3 = -3
2240 '                Break
2241 '                '
2242 '            Else
2243 '                MCnt% = MFinCnt%
2244 '                ScrewGet_S3 = 0
2245 '            EndIf
2246 '        Next  MCnt%
2247         '
2248         Ovrd 100
2249         Spd M_NSpd
2250         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2251         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
2252         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
2253         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2254         '������x�z���m�F
2255 ' �ȉ��b��폜
2256 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2257 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
2258 '            MCommentD1001 = 94
2259 '            MCommentD1002 = 95
2260 '            ScrewGet_S3 = -3
2261 '        EndIf
2262 '        If MRtn = 1 Then      '�z��OK�̏ꍇ
2263 '            ScrewGet_S3 = 1
2264 '        EndIf
2265 '        Break
2266     Else
2267         'M�l�W
2268         If MMScrewJudge% = 2 Then
2269             ScrewGet_S3 = -2
2270         EndIf
2271     EndIf
2272 FEnd
2273 '
2274 '��fnKEY_WAIT()
2275 ''' <summary>
2276 ''' GOT����̃L�[���͑҂�
2277 ''' </summary>
2278 '''<returns>1�F��~    2�F����
2279 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
2280 '''         5�FNG
2281 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
2282 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
2283 '''</returns>
2284 ''' <remarks>
2285 ''' Date   : 2021/07/07 : M.Hayakawa
2286 ''' </remarks>'
2287 Function M% fnKEY_WAIT()
2288     fnKEY_WAIT = 0
2289     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
2290     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
2291     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
2292     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2293     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
2294     Dly 0.2
2295     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
2296     MLocalLoopFlg=1
2297     While MLocalLoopFlg=1
2298         If M_In(11345) = 1 Then         '��~   M5345
2299             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
2300             fnKEY_WAIT = 1
2301             MLocalLoopFlg=-1
2302             Break
2303         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
2304             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
2305             fnKEY_WAIT = 2
2306             MLocalLoopFlg=-1
2307             Break
2308         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
2309             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
2310             fnKEY_WAIT = 3
2311             MLocalLoopFlg=-1
2312             Break
2313         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
2314             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
2315             fnKEY_WAIT = 4
2316             MLocalLoopFlg=-1
2317             Break
2318         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
2319             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
2320             fnKEY_WAIT = 5
2321             MLocalLoopFlg=-1
2322             Break
2323             '
2324         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
2325             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
2326             fnKEY_WAIT = MRobotInit1%
2327             MLocalLoopFlg=-1
2328             Break
2329             '
2330         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
2331             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
2332             fnKEY_WAIT = MRobotInit2%
2333             MLocalLoopFlg=-1
2334             Break
2335             '
2336         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
2337             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
2338             fnKEY_WAIT = MRobotInit3%
2339             MLocalLoopFlg=-1
2340             Break
2341             '
2342         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2343             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2344             fnKEY_WAIT = MRobotInit4%
2345             MLocalLoopFlg=-1
2346             Break
2347             '
2348         Else
2349         EndIf
2350     WEnd
2351     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2352     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2353 FEnd
2354 '
2355 '�� fnAUTO_CTL
2356 ''' <summary>
2357 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2358 ''' </summary>
2359 ''' <remarks>
2360 ''' Date   : 2021/07/07 : M.Hayakawa
2361 ''' </remarks>
2362 Function M% fnAUTO_CTL
2363     fnAUTO_CTL = 0
2364     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2365     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2366     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2367     '
2368     If M_Svo=0 Then             '�T�[�{ON�m�F
2369         Servo On
2370     EndIf
2371     Wait M_Svo=1
2372 FEnd
2373 '
2374 '�� fnWindScreenOpen
2375 ''' <summary>
2376 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2377 ''' </summary>
2378 '''<param name="%"></param>
2379 '''<param name="%"></param>
2380 '''<param name="%"></param>
2381 '''<param name="%"></param>
2382 ''' <remarks>
2383 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2384 ''' MWindReSet = 0     ��ʔ�\��
2385 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2386 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2387 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2388 ''' Date   : 2021/07/07 : M.Hayakawa
2389 ''' </remarks>
2390 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2391     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2392         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2393     EndIf
2394     '
2395     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2396         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2397     EndIf
2398     '
2399     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2400        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2401     EndIf
2402     '
2403     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2404     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2405     Dly 0.5
2406     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2407 FEnd
2408 '
2409 '��FnCtlValue2
2410 ''' <summary>
2411 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2412 ''' </summary>
2413 ''' <param name="MCtlNo%"></param>
2414 ''' <remarks>
2415 ''' Date : 2022/04/28 �n��
2416 ''' </remarks>
2417 '''
2418 '''  1�F������       �{�P
2419 '''  2�F�g���n�j��   �{�P
2420 '''  3�F�g���m�f��   �{�P (���g�p)
2421 '''  4�F�z���G���[�� �{�P
2422 ''' 99�F�Ǐ��J�n�M�� OFF
2423 '''
2424 Function M% FnCtlValue2(ByVal MCtlNo%)
2425     FnCtlValue2 = 1
2426     Select MCtlNo%
2427         Case 1        '�������{�P
2428             M_Out(12569) = 0             '�����݊J�n�M��OFF
2429             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2430             MInputQty = M_In16(11600)    '��������M
2431             MInputQty = MInputQty + 1    '�������{�P
2432             M_Out16(12592) = MInputQty   '���������M
2433             M_Out(12569) = 1             '�����݊J�n�M��ON
2434             Break
2435             '
2436         Case 2        '�g���n�j���{�P
2437             M_Out(12569) = 0             '�����݊J�n�M��OFF
2438             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2439             MAssyOkQty = M_In16(11616)   '�g��OK����M
2440             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2441             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2442             M_Out(12569) = 1             '�����݊J�n�M��ON
2443             Break
2444             '
2445         Case 4        '�z���G���[���{�P
2446             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2447             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2448             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2449             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2450             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2451             M_Out(12569) = 1                       '�����݊J�n�M��ON
2452             Break
2453             '
2454         Case 99        '�Ǐ��J�n�M��OFF
2455             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2456             M_Out(12569) = 0        '�����݊J�n�M��OFF
2457             Break
2458             '
2459     End Select
2460     Exit Function
2461 FEnd
2462 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2463 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2464 '-------------------------------------------------------------------------------
2465 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2466 '   ����
2467 '       PInspPos()      �F�����ʒu
2468 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2469 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2470 '       MInspCnt%       �F�����ʒu��
2471 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2472 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2473 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2474 '   �߂�l�F����
2475 '       0=�ُ�I���A1=����I��
2476 '
2477 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2478 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2479 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2480 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2481 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2482 '-------------------------------------------------------------------------------
2483     '----- �����ݒ� -----
2484     Cnt 0                                                           '�ړ�����������(�����l=0)
2485     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2486 '    Cnt 1,0.1,0.1
2487     '�ϐ��錾�E������
2488     Def Inte MNum                                                   '�����ԍ�(������1�`)
2489     MNum% = 1                                                       '�����ԍ������l�ݒ�
2490     Def Inte MEndFlg                                                '�����I���t���O
2491     MEndFlg% = 0
2492     '
2493     '����G�ԍ��ݒ�v���E�������s�v��off
2494     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2495     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2496     '�G���[�ԍ��N���A
2497     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2498     M_Out16(MOUT_InspErrNum) = MInspErrNum
2499     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2500     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2501     '
2502     'Insight Ready check?
2503     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2504         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2505         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2506         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2507         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2508         Exit Function
2509     EndIf
2510     '
2511     '�����ʒu���m�F
2512     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2513         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2514         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2515         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2516         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2517         Exit Function
2518     EndIf
2519     '
2520     '
2521     '
2522     '----- ���C������ -----
2523     '�ݒ肳�ꂽ�����ʒu�����̌������s
2524     While( MEndFlg% = 0 )
2525         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2526         MSetGrNumRetryExitFlg = 0
2527         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2528         While( MSetGrNumRetryExitFlg = 0 )
2529         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2530             '
2531             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2532             '
2533             '----- �����O���[�v�ԍ��ݒ� -----
2534             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2535             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2536             '
2537             '�����ʒu�ֈړ��E�ړ������҂�
2538             Mvs PInspPos( MNum% )                                       '�ړ�
2539             Dly 0.05                                                    '�ړ�������Delay
2540             '
2541             '�����O���[�v�ԍ��ݒ�I���m�F
2542             M_Timer(1) = 0
2543             MExitFlg = 0
2544             While( MExitFlg = 0 )
2545                 '����G�ݒ萳��I��?
2546                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2547                     MExitFlg = 1
2548                 '
2549                 '����G�ݒ�ُ�I��?
2550                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2551                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2552                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2553                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2554                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2555                     EndIf
2556                     MExitFlg = 1
2557                 '
2558                 'timeout�`�F�b�N
2559                 ElseIf 1000 < M_Timer(1) Then
2560                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2561                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2562                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2563                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2564                     EndIf
2565                     MExitFlg = 1
2566                 EndIf
2567             WEnd
2568             '
2569             '����G�ԍ��ݒ�v��off
2570             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2571             '
2572             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2573             'NG�Ȃ���Δ�����
2574             If MCurrentStepErr = 0 Then
2575                 MSetGrNumRetryExitFlg = 1
2576             Else
2577                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2578                 If MSetGrNumRetryCnt = 0 Then
2579                     MSetGrNumRetryExitFlg = 1
2580                 Else
2581                     'Retry�ց@���̑O��Delay
2582                     Dly 0.5
2583                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2584                 EndIf
2585             EndIf
2586             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2587             '
2588         WEnd
2589         '
2590         '
2591         '
2592         '----- �������s -----
2593         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2594             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2595                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2596                 MInspRetryExitFlg = 0
2597                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2598                 While( MInspRetryExitFlg = 0 )
2599                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2600                     '
2601                     '���������m�F
2602                     MRetryCnt = MRetryCnt - 1
2603                     M_Timer(1) = 0
2604                     MExitFlg = 0
2605                     While( MExitFlg = 0 )
2606                     '���������҂�
2607                         '����OK�I��?
2608                         If M_In( MIN_IS_InspOK% ) = 1  Then
2609                             MJudgeOKFlg = 1                         '����OK�t���OON
2610                             MExitFlg = 1
2611                         '
2612                         '����NG�I��?
2613                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2614                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2615                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2616                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2617                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2618                                 EndIf
2619                             EndIf
2620                             MExitFlg = 1
2621                         '
2622                         '�����ُ�I��(IS timeout)?
2623                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2624                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2625                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2626                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2627                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2628                                 EndIf
2629                             EndIf
2630                             MExitFlg = 1
2631                         '
2632                         'timeout�`�F�b�N
2633                         ElseIf 3000 < M_Timer(1) Then
2634                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2635                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2636                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2637                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2638                                 EndIf
2639                             EndIf
2640                             MExitFlg = 1
2641                         EndIf
2642                     WEnd
2643                     '
2644                     '�����J�n�v��off
2645                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2646                     '
2647                     'OK�Ȃ甲����
2648                     If MJudgeOKFlg = 1 Then
2649                         MInspRetryExitFlg = 1
2650                     Else
2651                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2652                         If MRetryCnt = 0 Then
2653                             MInspRetryExitFlg = 1
2654                         Else
2655                             'Retry�ց@���̑O��Delay
2656                             Dly 0.3
2657                         EndIf
2658                     EndIf
2659                     '
2660                 WEnd
2661             EndIf
2662         EndIf
2663         '
2664         '
2665         '
2666         MNum% = MNum% + 1                                           '����Step+1
2667         '�����I���m�F�@�����I���t���O�Z�b�g
2668         If (MInspCnt% < MNum% ) Then
2669             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2670         EndIf
2671         'NG���������s������
2672         If MInspErrNum <> 0 Then                                    'NG����?
2673             If MNgContinue% <> 1 Then                               'NG���s?
2674                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2675             EndIf
2676         EndIf
2677     WEnd
2678     '
2679     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2680     If 0 < MZAxis% Then
2681         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2682         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2683         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2684     EndIf
2685     Fine 0 , P
2686     '
2687     '�߂�l�ݒ�
2688     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
2689         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2690     Else
2691         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2692         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2693         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2694     EndIf
2695     '
2696 FEnd
2697 '
2698 ' ��ISInspection
2699 ''' <summary>
2700 ''' Insight�ɂ��摜�����������s
2701 ''' </summary>
2702 '''<param name="PInspPos()">�����ʒu</param>
2703 '''<param name="MInspGrNum%()">�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j</param>
2704 '''             PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2705 '''<param name="MInspCnt%">�����ʒu��</param>
2706 '''<param name="MZAxis%">�I������Z���ޔ����W�i-1:�����j</param>
2707 '''             �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2708 '''<param name="MNgContinue%">=1�Ō����G���[�ENG�������ɑSStep�̌������s��</param>
2709 '''<returns>    ���� 0=�ُ�I���A1=����I��</returns>
2710 '''         MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2711 '''         MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���"
2712 ''' <remarks>
2713 ''' Date   : 2021/07/07 : M.Hayakawa
2714 ''' </remarks>
2715 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2716 '    '�摜�g�p�m�F 0<- �摜�m�F�����̏ꍇ
2717 '    If M_In(11369) = 0 Then            'toRBT_�g�p�m�F
2718 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2719 '    EndIf
2720 ''
2721 '    Cnt 0                                                       '�ړ�����������(�����l=0)
2722 '    Fine 0.05,P                                                 '�ʒu���ߊ��������ݒu�@0.05mm
2723 '    MNum% = 1                                                   '�����ԍ������l�ݒ�
2724 '    Def Inte MEndFlg                                            '�����I���t���O
2725 '    MEndFlg% = 0
2726 '    '
2727 '    '�G���[�ԍ��N���A
2728 '    MInspErrNumSub = 0                                          '�������s�G���[�ԍ�sub
2729 '    MInspErrNum = 0                                             '�������s�G���[�ԍ�
2730 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2731 '    MInspNGStepNum = 0                                          '�������sNGStep�ԍ�
2732 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2733 '    '
2734 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready off�Ȃ�I��
2735 '        MInspErrNum = 20                                        '�������s�G���[�ԍ� 20 Insight offline
2736 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2737 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2738 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2739 ''
2740 '    EndIf
2741 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2742 '    '
2743 '    '�����ʒu���m�F
2744 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2745 '        MInspErrNum = 21                                        '�����f�[�^�Ȃ� 21�@����<1
2746 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2747 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2748 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2749 ''
2750 '    EndIf
2751 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2752 '    '
2753 '    '�ݒ肳�ꂽ�����ʒu�����̌������s
2754 '    While( MEndFlg% = 0 )
2755 '        '�����I���m�F�@�����I���t���O�Z�b�g
2756 '        If (MInspCnt% < MNum% ) Then
2757 '            MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2758 '        EndIf
2759 '        '
2760 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����J�n�@INSPTAST1
2761 '        If MEndFlg% = 0 Then
2762 '            M_01# = MInspGrNum%(MNum%)                          '����G�ԍ����n��
2763 '        EndIf
2764 '        M_02# = MEndFlg%                                        '�����I���t���O���n��
2765 '        M_05# = MNum%                                           '�����ԍ�(������1�`)
2766 '        '�^�X�N�@����G�ݒ�t���O���n��
2767 '        If MEndFlg% = 0 Then
2768 '            If 0 < MInspGrNum%(MNum%) Then
2769 '                M_03# = 1
2770 '            Else
2771 '                M_03# = 0
2772 '            EndIf
2773 '        Else
2774 '            M_03# = 0
2775 '        EndIf
2776 '        '�^�X�N�@�������ʊm�F�t���O���n��
2777 '        If 1 < MNum% Then
2778 '            If 0 < MInspGrNum%(MNum%-1) Then
2779 '                M_04# = 1
2780 '            Else
2781 '                M_04# = 0
2782 '            EndIf
2783 '        Else
2784 '            M_04# = 0
2785 '        EndIf
2786 '        '
2787 '        '�^�X�N�����J�n
2788 '        M_00# = 1                                               'TASK�����J�n
2789 '        '�^�X�N�����J�n�m�F
2790 '        M_Timer(1) = 0
2791 '        MExitFlg = 0
2792 '        While( MExitFlg = 0 )
2793 '            '�����J�n�����m�F
2794 '            If M_00# = 0 And M_10# = 8 Then
2795 '                MExitFlg = 1
2796 '            EndIf
2797 '            'timeout�`�F�b�N
2798 '            If 2000 < M_Timer(1) Then
2799 '                If MNgContinue% = 1 Then                        'NG���s?
2800 '                    MInspErrNumSub = 36                         '�G���[�ԍ��ݒ�36
2801 '                Else
2802 '                    MInspErrNum = 36                            '�G���[�ԍ��ݒ�36
2803 '                EndIf
2804 '                MExitFlg = 1
2805 '            EndIf
2806 '        WEnd
2807 '        '
2808 '        '�����ʒu�ֈړ��E�ړ������҂�
2809 '        If 0 = MInspErrNum Then
2810 '            If MEndFlg% = 0 Then
2811 '                Mvs PInspPos( MNum% )                           '�ړ�
2812 '            EndIf
2813 '        EndIf
2814 '        '
2815 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����I���҂��@INSPTAST1
2816 '        If 0 = MInspErrNum Then
2817 '            M_Timer(1) = 0
2818 '            MExitFlg = 0
2819 '            While( MExitFlg = 0 )
2820 '                '���������҂��i����I���j
2821 '                If M_10# = 1 Then
2822 '                    MExitFlg = 1
2823 '                EndIf
2824 '                '���������҂��i�ُ�I���j
2825 '                If M_10# = 0 Then
2826 '                    If MNgContinue% = 1 Then                    'NG���s?
2827 '                        MInspErrNumSub = M_12#                  '�G���[�ԍ��ݒ�@M12
2828 '                    Else
2829 '                        MInspErrNum = M_12#                     '�G���[�ԍ��ݒ�@M12
2830 '                    EndIf
2831 '                    MExitFlg = 1
2832 '                EndIf
2833 '                'timeout�`�F�b�N
2834 '                If 5000 < M_Timer(1) Then
2835 '                    If MNgContinue% = 1 Then                    'NG���s?
2836 '                        MInspErrNumSub = 31                     '�G���[�ԍ��ݒ�31
2837 '                    Else
2838 '                        MInspErrNum = 31                        '�G���[�ԍ��ݒ�31
2839 '                    EndIf
2840 '                    MExitFlg = 1
2841 '                EndIf
2842 '            WEnd
2843 '        EndIf
2844 '        '
2845 '        '�������ʊm�F
2846 '        If 0 = MInspErrNum Then
2847 '            If 1 < MNum% Then
2848 '                If 0 < MInspGrNum%(MNum%-1) Then                '��������?
2849 '                    If M_11# = 2 Then                           '����NG?
2850 '                        If MNgContinue% = 1 Then                'NG���s?
2851 '                            If MInspNGStepNum = 0 Then          'NG������?
2852 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2853 '                            EndIf
2854 '                            MInspErrNumSub = 32                 '�G���[�ԍ��ݒ� 32:����NG
2855 '                        Else
2856 ''                            MInspNGStepNum = MNum% - 1          '�������sNGStep�ԍ��ݒ�
2857 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2858 '                            MInspErrNum = 32                    '�G���[�ԍ��ݒ� 32:����NG
2859 '                        EndIf
2860 '                   EndIf
2861 '                EndIf
2862 '            EndIf
2863 '        EndIf
2864 '        '
2865 '        '�G���[�Ȃ猟�����f�I������̂�Loop���甲���邽�ߏI���t���O�Z�b�g
2866 '        If 0 <> MInspErrNum Then
2867 '            MEndFlg% = 1
2868 '        EndIf
2869 '        '
2870 '        '�������s�A�捞�����҂�
2871 '        If 0 = MInspErrNum Then
2872 '            If MEndFlg% = 0 Then
2873 '                If 0 < MInspGrNum%(MNum%) Then                  '��������?
2874 '                    M_Out(MOUT_IS_Insp%) = 1                    '�������s�v��on
2875 '                    '�捞�����m�F
2876 '                    M_Timer(1) = 0
2877 '                    MExitFlg = 0
2878 '                    While( MExitFlg = 0 )
2879 '                        '���������҂�
2880 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2881 '                            MExitFlg = 1
2882 '                        EndIf
2883 '                        'timeout�`�F�b�N
2884 '                        If 2000 < M_Timer(1) Then
2885 '                            If MNgContinue% = 1 Then            'NG���s?
2886 '                                MInspErrNumSub = 33             '�G���[�ԍ��ݒ�33
2887 '                            Else
2888 '                                MInspErrNum = 33                '�G���[�ԍ��ݒ�33
2889 '                            EndIf
2890 '                            MExitFlg = 1
2891 '                        EndIf
2892 '                    WEnd
2893 '                EndIf
2894 '                '
2895 '            EndIf
2896 '        EndIf
2897 '        MNum% = MNum% + 1
2898 '    WEnd
2899 '    '
2900 '    '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2901 '    If 0 < MZAxis% Then
2902 '        PCurrentPos = P_Curr                                    '���݈ʒu�擾
2903 '        PCurrentPos.Z = MZAxis%                                 'Z����ݒ�
2904 '        Mvs PCurrentPos                                         '���݈ʒu���ֈړ�
2905 '    EndIf
2906 '    '
2907 '    'NG���s������
2908 '    If MNgContinue% = 1 Then                                    'NG���s?
2909 '        MInspErrNum = MInspErrNumSub                            '�G���[�ԍ��ݒ�
2910 '    EndIf
2911 '    '
2912 '    '�߂�l�ݒ�
2913 '    If MInspErrNum = 0 Then
2914 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2915 '    Else
2916 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2917 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2918 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2919 '    EndIf
2920 '    '
2921 '*ISInspection_End
2922 'FEnd
2923 '
2924 '��InitialZoneB
2925 ''' <summary>
2926 ''' ����~��̕��A����
2927 ''' 1)���ޔ��@Z������Ɉړ�
2928 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
2929 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
2930 ''' 4)�C�j�V�����|�W�V�����ֈړ�
2931 ''' </summary>
2932 ''' <remarks>
2933 ''' Date : 2022/04/08 : N.Watanabe
2934 ''' </remarks>
2935 Function V fnInitialZoneB()
2936     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/04/26 �n��
2937 '
2938 '�p�����[�^
2939     Ovrd 5
2940 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2941 '    Cmp Pos, &B100011
2942 '
2943 '���A����J�n
2944 '
2945 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������
2946 *RecoveryChuckOpen
2947     PActive = P_Curr          '���݈ʒu���擾
2948     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
2949 'PProductOnRoboSet(�˂����{���i�u���ʒu)�́A�`���b�N���
2950     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2951         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2952             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2953                 MRecoveryChuckOpen = 1
2954             EndIf
2955         EndIf
2956     EndIf
2957 'PProductOnRoboGet(�˂����{���i���ʒu)�́A�`���b�N���
2958     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2959         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2960             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2961                 MRecoveryChuckOpen = 1
2962             EndIf
2963         EndIf
2964     EndIf
2965 '
2966     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2967     M_Out(12256) = 0                           '�{�̃`���b�N��OFF
2968     M_Out(12257) = 1                           '�{�̃`���b�N�JON
2969 '
2970     M_20# = 0                                  'KEY���͏�����
2971     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
2972     If MRtn = 1 Then M_Out(12257) = 0          '�{�̃`���b�N�JOFF
2973     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2974 '
2975     fErrorProcess(11,244,284,0)
2976     If M_20# = MNext% Then M_20# = MClear%
2977     If M_20# = MAbout% Then GoTo *RecoveryEnd
2978     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2979     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2980 '
2981     *RecoveryChuckOpenEnd
2982 '
2983 '�w�ʔ��
2984 'PPlateBackSet�`PPlateBackSet_6�̃G���A�ɂ���Ƃ��́A�{�̃`���b�N�J��
2985 '�EPPlateBackSet_6         '�o�H6
2986 '�EPPlateBackSet_5         '�o�H7
2987 '�EPPlateBackSet_4         '�o�H8
2988 '�EPPlateBackSet_3         '�o�H9
2989 '�EPPlateBackSet_2         '�o�H10
2990 '�EPPlateBackSet_1         '�o�H11
2991 '�EPPlateBackSet           '�w�ʔu���ʒu
2992 '��L�V�_�̂w���W�E�x���W�E�y���W��J6�������LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2993     PActive = P_Curr                    '���݈ʒu���擾
2994     JActive = J_Curr                    '���݈ʒu���擾
2995     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2996     If (PActive.X >= -35) And (PActive.X <= -5) Then
2997         If (PActive.Y >= 340) And (PActive.Y <= 510) Then
2998             If (PActive.Z >= 470) And (PActive.Z <= 560) Then
2999                 If (MJ6 >= 160) And (MJ6 <= 200) Then
3000                     M_Out(12256) = 0            '�{�̃`���b�N��OFF
3001                     M_Out(12257) = 1            '�{�̃`���b�N�JON
3002                 Dly 1.0
3003                 EndIf
3004             EndIf
3005         EndIf
3006     EndIf
3007 '
3008 '
3009 '�������@���ځA���ޔ����o���Ȃ����̑Ώ�
3010 '
3011     Ovrd 1
3012 'PProductOnRoboSet(Get)�`PProductOnRoboSet(Get)_2�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_2��
3013 '�EPProductOnRoboSet
3014 '�EPProductOnRoboSet_1
3015 '�EPProductOnRoboSet_2
3016 '�EPProductOnRoboGet
3017 '�EPProductOnRoboGet_1
3018 '�EPProductOnRoboGet_2
3019 '��L�U�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
3020     PActive = P_Curr                    '���݈ʒu���擾
3021     JActive = J_Curr                    '���݈ʒu���擾
3022     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
3023     If (PActive.X >= -35) And (PActive.X <= 0) Then
3024         If (PActive.Y >= 350) And (PActive.Y <= 420) Then
3025             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
3026                 If (MJ6 >= -20) And (MJ6 <= 20) Then
3027                     Mvs PProductOnRoboSet_1
3028                     Dly 1.0
3029                     Mvs PProductOnRoboSet_2
3030                     Dly 1.0
3031                     Mov PProductOnRoboSet_3
3032                     Dly 1.0
3033                 EndIf
3034             EndIf
3035         EndIf
3036     EndIf
3037 '
3038 'PProductOnRoboSet(Get)_2�`PProductOnRoboSet(Get)_3�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_3��
3039 '�EPProductOnRoboSet_2
3040 '�EPProductOnRoboSet_3
3041 '�EPProductOnRoboGet_2
3042 '�EPProductOnRoboGet_3
3043 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
3044     PActive = P_Curr                    '���݈ʒu���擾
3045     JActive = J_Curr                    '���݈ʒu���擾
3046     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
3047     If (PActive.X >= -35) And (PActive.X <= 0) Then
3048         If (PActive.Y >= 280) And (PActive.Y <= 390) Then
3049             If (PActive.Z >= 410) And (PActive.Z <= 570) Then
3050                 If (MJ6 >= -20) And (MJ6 <= 20) Then
3051                     Mvs PProductOnRoboSet_3
3052                     Dly 1.0
3053                 EndIf
3054             EndIf
3055         EndIf
3056     EndIf
3057 '
3058     Ovrd 5
3059 '
3060 '���ޔ�
3061     PActive = P_Curr
3062     Pmove = PActive
3063     Pmove.Z = 640           '���ޔ�����ꗥ�̍���
3064     If PActive.X > 550 Then
3065         Pmove.Z =550        '�p���b�g��ɘr��L�΂��Ă���Ƃ���640�܂ŏグ���Ȃ��ׁA��O���u
3066     EndIf
3067     If PActive.Z < Pmove.Z Then
3068         Mvs Pmove
3069     EndIf
3070     Dly 1.0
3071 'J1���ȊO��ޔ��|�W�V�����ֈړ�
3072     JActive = J_Curr
3073     Jmove = JTaihi
3074     Jmove.J1 = JActive.J1        'J1���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
3075     Jmove.J6 = JActive.J6        'J6���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
3076     Mov Jmove
3077     Dly 1.0
3078 'J1���݂̂�ޔ��|�W�V�����ֈړ�
3079     Mov JTaihi
3080     Dly 1.0
3081 '�C�j�V�����|�W�V�����ֈړ�
3082     Mov PInitialPosition
3083     Cmp Off
3084     Ovrd 100
3085 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n
3086     If M_In(11856) = 0 Then                 ' ��~���̂�
3087         fnAutoScreenComment(501)            ' ��ԕ\��[�l�W���ߋ@�����^�]�J�n��] 2022/04/25 �n��
3088         M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
3089         MRet = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
3090         If MRet = 0 Then
3091         Else
3092             M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
3093         EndIf
3094     EndIf
3095     M_Out(12262) = 0            '�ʒu���ߏoOFF
3096     M_Out(12263) = 1            '�ʒu���ߖ�ON
3097     fErrorProcess(11,253,281,0)
3098 *RecoveryEnd
3099     Exit Function
3100 FEnd
3101 '
3102 '
3103 '��fnAutoScreenComment
3104 ''' <summary>
3105 ''' ���C����ʂ̓���󋵕\��
3106 ''' �R�����gD1005�̐ݒ�
3107 ''' </summary>
3108 '''<param name="McommentD1005%">�R�����gID</param>
3109 ''' <remarks>
3110 ''' Date   : 2021/07/07 : M.Hayakawa
3111 ''' </remarks>
3112 Function fnAutoScreenComment(ByVal McommentD1005%)
3113     M_Out16(12576) = McommentD1005%
3114 FEnd
3115 '
3116 '��fnRoboPosChk
3117 ''' <summary>
3118 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
3119 ''' </summary>
3120 '''<param name="MINNumber%">���͔ԍ�</param>
3121 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3122 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3123 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
3124 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
3125 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3126 ''' <remarks>
3127 ''' Date   : 2021/07/07 : M.Hayakawa
3128 ''' </remarks>
3129 Function M% fnRoboPosChk
3130     fnRoboPosChk = 0
3131     MRet = fnStepRead()
3132     '�����ʒu�łȂ��Ɣ��f�����ꍇ
3133     '�E�B���h��ʐ؊���
3134     If MRBTOpeGroupNo > 5 Then
3135         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3136         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3137         Dly 0.2
3138         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3139         Dly 1.5
3140         '
3141         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3142         '
3143         MLoopFlg% = 1
3144         While MLoopFlg% = 1
3145             '
3146             '
3147             MKeyNumber% = fnKEY_WAIT()
3148             Select MKeyNumber%
3149                 Case Is = MAbout%       '��~
3150                     M_20# = MAbout%
3151                     MLoopFlg% = -1
3152                     Break
3153                 Case Is = MNext%        '����
3154                     'MLoopFlg% = -1
3155                     Break
3156                 Case Is = MContinue%    '�p��
3157                     M_20# = MContinue%
3158                     MLoopFlg% = -1
3159                     Break
3160                 Default
3161                     Break
3162             End Select
3163         WEnd
3164     EndIf
3165     '
3166     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
3167         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3168         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
3169         Select MRBTOpeGroupNo
3170             Case Is = 5                          '�������Ȃ�
3171                 Break
3172             Case Is = 10                         '�����ʒu�֖߂�
3173                 'Mov PTEST001
3174                 Break
3175             Case Is = 15                         '�����ʒu�֖߂�
3176                 'Mov PTEST002
3177                 Dly 0.5
3178                 'Mov PTEST001
3179                 Dly 0.5
3180                 Break
3181             Default
3182                 Break
3183         End Select
3184         '
3185         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
3186         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
3187         MRBTOpeGroupNo = 5
3188         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
3189         Dly 1.0
3190         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
3191         fnRoboPosChk = 1                        '�����ʒu������s
3192         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3193     EndIf
3194     Exit Function
3195 FEnd
3196 '
3197 '��frInCheck
3198 ''' <summary>
3199 ''' �Z���T�[IN�`�F�b�N
3200 ''' </summary>
3201 '''<param name="MINNumber%">���͔ԍ�</param>
3202 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3203 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3204 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3205 ''' <remarks>
3206 ''' Date   : 2021/07/07 : M.Hayakawa
3207 ''' </remarks>
3208 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3209     M_Timer(4) = 0
3210     MloopFlg = 0
3211     While MloopFlg = 0
3212         MCrtTime& = M_Timer(4)
3213         If M_In(MINNumber%) = MCMPFLG% Then
3214             MloopFlg = 1
3215             frInCheck = 1
3216         ElseIf MCrtTime& > MTimeCnt& Then
3217             MloopFlg = 1
3218             frInCheck = 0
3219         EndIf
3220     WEnd
3221 FEnd
3222 '-----------------------------------------------
3223 '
3224 '�˂����ߋ@�ʐM�m�F
3225 '
3226 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3227 'fScrewTcomChk = 0�@�F����I��
3228 '          �@ �@ -1 �F�ُ�I��
3229 '-----------------------------------------------
3230 Function M% fScrewTcomChk
3231 *ReCheckScewTcomChk
3232     fScrewTcomChk = 0
3233     '�ʐM�m�F���M
3234     M_Out(MOUT_ScwT_ComChk%) = MOn%
3235     '�ʐM�m�F��M�ҋ@
3236 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3237     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3238     '�ʐM�m�F���M�I��
3239     M_Out(MOUT_ScwT_ComChk%) = MOff%
3240     If MRtn = 0 Then
3241         fScrewTcomChk = -1
3242     EndIf
3243     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
3244  '
3245 FEnd
3246 '
3247 '
3248 '-----------------------------------------------
3249 '
3250 '�˂����ߊJ�n���M
3251 '
3252 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3253 'fScrewTStart = 0�@�F����I��
3254 '           �@�@-1 �F�ُ�I��
3255 '-----------------------------------------------
3256 Function M% fScrewTStart
3257     fScrewTStart = 0
3258     nRet% = 0
3259     '�˂����ߊJ�n�ҋ@����M
3260 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3261     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3262     If MRtn = 0 Then nRet% = -1
3263     If MRtn = 0 Then GoTo *ScrewStartERROR      '�J�n�ł��Ȃ������ꍇ�W�����v
3264     Dly 0.1
3265     '�˂����ߊJ�n��M�𑗐M
3266     M_Out(MOUT_ScwT_ST%) = MOn%
3267     Dly 0.5
3268     'Wait M_In(MTEST_KEY%) = MOn%
3269     '�˂����ߊJ�n���M�I��
3270     M_Out(MOUT_ScwT_ST%) = MOff%
3271     '
3272 *ScrewStartERROR
3273     fScrewTStart = nRet%
3274 FEnd
3275 '
3276 '
3277 '
3278 '-----------------------------------------------
3279 '
3280 '�˂����ߊ�����M
3281 '
3282 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3283 'fScewTFinish = 0�@�F����I��
3284 '          �@ �@-1 �F�ُ�I��
3285 '-----------------------------------------------
3286 Function M% fScewTFinish
3287 *ReCheckScewTFinish
3288     fScewTFinish = 0
3289     '�˂����ߊ����ҋ@����M
3290 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3291     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3292     If MRtn = 0 Then
3293         fScewTFinish = -1
3294     EndIf
3295     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3296     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3297     Dly 0.1
3298     '�˂����ߊ�����M�𑗐M
3299     M_Out(MOUT_ScwT_FinOK%) = MOn%
3300     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3301     '�˂����ߊJ�n���M�I��
3302     M_Out(MOUT_ScwT_FinOK%) = MOff%
3303     'Wait M_In(MTEST_KEY%) = MOn%
3304     '
3305 *ScewTFinish_ErrEnd
3306 FEnd
3307 '
3308 '
3309 '-----------------------------------------------
3310 '
3311 '����xx��~��M
3312 '
3313 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3314 'fScewTCaseStop = 0�@�F����I��
3315 '          �@   �@-1 �F�ُ�I��
3316 '-----------------------------------------------
3317 Function M% fScewTCaseStop(ByVal MCase%())
3318 *ReCheckScewTCaseStop
3319     fScewTCaseStop = 0
3320     '����xx��~����M
3321     Wait M_In(MCase%(1)) = MOn%
3322     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3323     If MRtn = 0 Then
3324         fScewTCaseStop = -1
3325     EndIf
3326     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3327     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3328     Dly 0.1
3329     '����xx��~��M�𑗐M
3330     M_Out(MCase%(2)) = MOn%
3331     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3332     '�˂����ߊJ�n���M�I��
3333     M_Out(MCase%(2)) = MOff%
3334 *ScewTCaseStop_ErrEnd
3335     '
3336 FEnd
3337 '
3338 '��fScrewTighenRoboCheck
3339 '<summary>
3340 '�˂����{�Ď�
3341 '</summary>
3342 '<param name = "MStopNum%"> ��~�ԍ�</param>
3343 '<returns>���� 0:�˂����{�ُ�I�� 1:OK </returns>
3344 '<make>
3345 '2021/12/2 �����V��
3346 '</make>
3347 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3348     fnAutoScreenComment(503)    '��ԕ\��[�˂����{����I���҂�] 2022/04/26 �n��
3349     fScrewTighenRoboCheck = 1
3350     MScrewTighenRoboFlg% = 1    '�t���O�̏�����
3351     MCheck% = 0
3352     While MScrewTighenRoboFlg% = 1
3353         MCheck% = M_In16(11904)
3354         If M_In(MStopNum%) = 1 Then '��~�ʒu�܂ŗ�����
3355             MScrewTighenRoboFlg% = 0 '�֐��𔲂���
3356             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
3357         EndIf
3358         If MCheck% <> 0 Then
3359             fScrewTighenRoboError(MCheck%)
3360             Select M_20#
3361                 Case MAbout%            '��~�������ꂽ�ꍇ
3362                     M_Out(12869) = 1 Dly 1.0
3363                     MScrewTighenRoboFlg% = 0
3364                     fScrewTighenRoboCheck = 0   '�ُ�I��
3365                     Break
3366                 Case MNgProcess%        'NG�������ꂽ�ꍇ
3367                     M_Out(12873) = 1 Dly 1.0
3368                     MScrewTighenRoboFlg% = 0
3369                     fScrewTighenRoboCheck = 0   '�ُ�I��
3370                     Break
3371                 Case MContinue%             '���g���C�������ꂽ�ꍇ
3372                     M_20# = MClear%         'M_20#������
3373                     M_Out(12871) = 1 Dly 1.0
3374                     Break
3375                 Case MNext%                 '���ւ������ꂽ�ꍇ
3376                     M_20# = MClear%         'M_20#������
3377                     M_Out(12874) = 1 Dly 1.0
3378                     Break
3379             End Select
3380             Dly 0.5
3381         EndIf
3382     WEnd
3383 FEnd
3384 '
3385 '��fScrewTighenRoboError
3386 '<summary>
3387 '�˂����{�G���[����
3388 '</summary>
3389 '<param name = "ErrorCode%"> �G���[�ԍ�</param>
3390 '<make>
3391 '2021/12/2 �����V��
3392 '</make>
3393 Function fScrewTighenRoboError(ByVal MErrorCode%)
3394     MErrorScreenCode% = 0
3395     MErrorScreenCode% = MErrorCode% + 300
3396     fErrorProcess(11,MErrorScreenCode%,0,0)
3397 FEnd
3398 '
3399 '��fErrorProcess
3400 '<summary>
3401 '�G���[����
3402 '</summary>
3403 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
3404 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
3405 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
3406 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
3407 '<make>
3408 '2021/11/5 �����V��
3409 '</make>
3410 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3411     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
3412     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
3413     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
3414     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
3415 *RETRY_ERR_PROCESS
3416      M_20# = MClear%     '������
3417 '        '�G���[�����L�q
3418         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3419 '        'GOT KEY���͑҂�
3420         MKeyNumber = fnKEY_WAIT()
3421 '        '
3422         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
3423             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3424             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3425             Break
3426          '
3427         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
3428             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3429             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3430         '
3431         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
3432             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3433             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3434          '
3435         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
3436             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3437             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3438             Break
3439         '
3440         EndIf
3441         '
3442         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3443 FEnd
3444 '
3445 '��fnTorqueCheck
3446 ''' <summary>
3447 ''' �g���N�`�F�b�N����p�̃��C��
3448 ''' </summary>
3449 ''' <remarks>
3450 ''' Date   : 2021/12/21 : H.AJI
3451 ''' </remarks>'
3452 Function M% fnTorqueCheck
3453     '�g���N�`�F�b�N�����M  �����n��~
3454     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3455     '
3456     fnTorqueCheck = 0
3457     Ovrd 20
3458     Mov PInitialPosition              '�����ʒu�ړ�
3459     Ovrd 100
3460     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3461     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3462     Dly 0.2
3463     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3464     '
3465     'M6340  �g���N�`�F�b�N��M
3466     'Dly 5.0
3467     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3468     Dly 1.0
3469     M_Out(12340) = 0
3470     '
3471     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3472     M_Out(12835) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3473    Wait M_In(11843) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3474     M_Out(12835) = 0                         '�˂����{�g���N�`�F�b�N��ʐؑ֊���
3475     '
3476     '
3477     MLoopFlg = 1
3478     While MLoopFlg = 1
3479         '
3480         Mov PInitialPosition              '�����ʒu�ړ�
3481         '
3482         MKeyNumber = fnKEY_WAIT()
3483         Select MKeyNumber
3484             Case Is = 1           '��~
3485                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3486                 Dly 1.0
3487                 M_Out(12343) = 0
3488                 Ovrd 20
3489                 'Mov PTicketRead_1
3490                 M_Out(12840) = 1          '�g���N�`�F�b�N�I��
3491                 Wait M_In(11859) = 1      '�˂����{����̏I��
3492                 M_Out(12840) = 0          '�g���N�`�F�b�N�I��
3493                 Ovrd 100
3494                 M_20# = 1
3495                 MLoopFlg = -1
3496                 Break
3497             Case Is = 2           '����
3498                 Break
3499             Case Is = 3           '�p��
3500                 Break
3501             Case Is = 4           '�g���N�`�F�b�N�J�n
3502                 M_Out(12342) = 1          '�g���N�`�F�b�N�J�n�v����M M6342
3503                 Dly 1.0
3504                 M_Out(12342) = 0
3505                 If M_In(11862) = 1 Then             '�g���N�`�F�b�J�[�m�F
3506                     fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3507                     MRet = fnScrewMTorque()           '�˂����{�p�g���N�`�F�b�N
3508                 EndIf
3509                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3510                 'MRet = fnMoveTorquePosi()
3511                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3512                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3513                 Break
3514             Default
3515                 Break
3516         End Select
3517     WEnd
3518     '
3519     '�g���N�`�F�b�N����~���M
3520     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3521     '
3522     '���{�b�g�̈ʒu�����ɖ߂�
3523     '
3524     '
3525  FEnd
3526  '
3527 '
3528 '
3529 '---------------------------
3530 '
3531 '    ���C����ʂ̕\���A��\���ݒ�
3532 '         �R�����gD1001, D1002, D1003�̐ݒ�
3533 '           MWindReSet = 0     ��ʔ�\��
3534 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3535 '           MWindErrScr = 10    �G���[��� D1001, D1002
3536 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3537 '
3538 '---------------------------
3539 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3540     fnMainScreenOpen = 0
3541     '
3542    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3543         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3544     EndIf
3545     '
3546     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3547         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3548     EndIf
3549     '
3550     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3551         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3552     EndIf
3553     '
3554     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3555     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3556     Dly 0.5
3557     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3558 FEnd
3559 '
3560 '��Main
3561 ''' <summary>
3562 ''' �g���N�`�F�b�N������
3563 ''' </summary>
3564 ''' <remarks>
3565 ''' Date   : 2021/12/21 : H.AJI
3566 ''' </remarks>'
3567 Function M% fnScrewMTorque
3568     fnScrewMTorque = 0
3569     M_Out(12838) = 1                         '�g���N�`�F�b�N�J�n1
3570     Wait M_In(11857) = 1                     '��M����
3571     M_Out(12838) = 0                         '�g���N�`�F�b�N�J�n1
3572     Dly 2.0
3573 FEnd
3574 '
3575 '----------------------------------------------------------------
3576 'fTimeOutJudge
3577 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3578 '����
3579 'Address% = �Ď��A�h���X�ԍ�
3580 'JudgeFlg% = �ΏۃA�h���X�̐���I�����̒l
3581 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3582 '�߂�l = 0 �G���[
3583 '         1 ����I��
3584 '         2 ���g���C
3585 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3586 '�쐬��
3587 '2022/9/20 ����
3588 '----------------------------------------------------------------
3589 '
3590 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3591     fTimeOutJudge = 0
3592     MJudge% = 1
3593     MRtn = 0
3594     M_20# = MClear%
3595     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3596 *TimeOutLoop
3597     If MRtn = 1 Then GoTo *TimeOut
3598         fErrorProcess(11,202,203,0)
3599         If M_20# = MNext% Then GoTo *TimeOutLoop
3600         If M_20# = MContinue% Then MJudge% = 2
3601         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3602 *TimeOut
3603     fTimeOutJudge = MJudge%
3604 '
3605 *JUDGE_ERROR_END
3606 FEnd
3607 '��Main
3608 ''' <summary>
3609 ''' �g������p�̃��C��
3610 ''' </summary>
3611 ''' <remarks>
3612 ''' Date   : 2021/07/07 : M.Hayakawa
3613 ''' </remarks>'
3614 Function Main
3615     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3616     '
3617     If M_Svo=0 Then
3618         Servo On
3619     EndIf
3620     Wait M_Svo=1
3621 '�g���X�^�[�g���t�����v���p���XON
3622     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3623 '�p�g���C�g����
3624     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3625     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3626     '
3627     M_20# = 0                                   'KEY���͏�����
3628     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
3629     MRet% = 0
3630 '�����ʒu�̊m�F�ƈړ�
3631 '
3632 '���A����@���s�E�����s����      2022/04/08 �n�� �쐬
3633     PActive = P_Curr                    '���݈ʒu���擾
3634     MRecoveryPass% = 0
3635     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3636         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3637             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3638                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3639             EndIf
3640         EndIf
3641     EndIf
3642     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3643         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3644             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3645                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3646             EndIf
3647         EndIf
3648     EndIf
3649     If MRecoveryPass% = 0 Then
3650        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3651     EndIf
3652 '
3653 '
3654 '    MRet% = fnRoboPosChk()
3655     If MRet% = 1 Then                           '�����ʒu�̓�����s�����ꍇ
3656         fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  '�E�B���h���
3657         MKeyNumber% = fnKEY_WAIT()
3658         Select MKeyNumber%
3659             Case Is = MAbout%       '��~
3660                 M_20# = MAbout%
3661                 MLoopFlg% = -1
3662                 Break
3663             Case Is = MNext%        '����
3664                 'MLoopFlg = -1
3665                 Break
3666             Case Is = MContinue%    '�p��
3667                 M_20# = MContinue%
3668                 MLoopFlg% = -1
3669                 Break
3670             Default
3671                 Break
3672         End Select
3673     EndIf
3674     '
3675     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3676         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3677 '�g���N�`�F�b�N
3678         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3679             MRet% = fnTorqueCheck()
3680             Break
3681         Else
3682 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
3683 '                MRtn = InspInit()               '�摜��������������
3684 '            EndIf
3685 '
3686             M_20# = MClear%             '������
3687 '�g���J�n
3688             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3689                 fnAssyStart()
3690             Else
3691                 M_20# = MPass%
3692             EndIf
3693 '�g���I�����t����
3694             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3695             Wait M_In(11572) = 1            '���t�擾����
3696             Dly 0.1
3697             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3698 '���t�^�[���j�b�g�ւ�OUT
3699             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3700             fnAutoScreenComment(89)         'AUTO��� �g����������
3701             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3702 'OK/NG�t���O�o��
3703             If M_20# <= 0 Then
3704                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3705             ElseIf M_20# = MPass% Then
3706                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3707             EndIf
3708 'PIAS�ɑg������������
3709             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3710                 If M_20# = MPass% Then
3711                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3712                 Else
3713                     'KEY���͂�NG�̏ꍇ
3714                     If M_20# = MNgProcess% Then
3715                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3716                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3717                         MRet% = fnPiasWrite(MNG%)
3718                        nAssyNgQty = nAssyNgQty + 1
3719                     EndIf
3720                     '
3721                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3722                     If M_20# = MAssyOK% Then
3723                             '-----------------------
3724                             'D732 -> D2600 �R�s�[�v��
3725                             M_Out(12566) = 1
3726 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3727                             M_Out(12566) = 0
3728                             '
3729                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3730                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3731                             '��ԍ��ƍ�(PP�͖��g�p�j
3732 '                            MRet% = fnPCBNumberCheck()
3733                         Else
3734                             MRet% = 1
3735                         EndIf
3736                         '
3737                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3738                             If M_20# <> MAbout% Then
3739                                 '�H������OK��������
3740                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3741                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3742                                 MRet% = fnPiasWrite(MOK%)
3743                                 nAssyOkQty = 0
3744                                 nAssyOkQty = nAssyOkQty + 1
3745                             Else
3746                                 nAssyOkQty = nAssyOkQty + 1
3747                             EndIf
3748                         EndIf
3749                     EndIf
3750 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3751 '                    MRet% = fnPiasWrite(MOK%)
3752                 EndIf
3753             Else
3754                 nAssyOkQty = nAssyOkQty + 1
3755             EndIf
3756             '
3757             '�g���I�����t��������
3758             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3759             '�������A�g��OK���A�g��NG��������
3760 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3761             '
3762 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3763 '                '�摜�����I������
3764 '                MRtn = InspQuit()
3765 '            EndIf
3766         EndIf
3767         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3768     EndIf
3769 '�p�g���C�g����
3770     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3771     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3772 'GOT�\��
3773     fnAutoScreenComment(93)  'AUTO��� �H������
3774 FEnd
3775 End
3776 '
3777 '���܂��Ȃ��R�����g
3778 '��΍폜�����
3779 '
3780 '
3781 '
3782 '
3783 '
3784 '
3785 '
JActive=(96.330,-32.290,124.210,0.000,88.080,180.000,0.000,0.000)
Jmove=(96.330,-46.870,111.640,0.000,80.580,180.000,0.000,0.000)
JTaihi=(0.000,-46.870,111.640,0.000,80.580,0.000,0.000,0.000)
PActive=(602.000,-150.000,550.000,-180.000,0.000,90.000,0.000,0.000)(7,0)
PInitialPosition=(340.000,0.000,580.000,-180.000,0.000,180.000)(7,0)
PMechaGet=(-415.660,-8.740,299.320,179.450,-2.220,176.860)(7,1048577)
PMechaGet_1=(-415.660,-8.740,409.960,179.450,-2.220,176.860)(7,1048577)
PMechaGet_2=(-189.840,-0.010,629.060,-180.000,0.000,-179.990)(7,1)
PMechaGet_3=(0.010,189.840,629.070,-180.000,0.000,90.000)(7,0)
PMechaGet_4=(327.500,0.020,596.240,-179.990,0.000,103.500)(7,0)
PMechaGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaSet1=(159.800,-334.910,320.000,122.020,86.640,31.390)(6,0)
PMechaSet1_1=(159.800,-334.910,340.000,122.020,86.640,31.390)(6,0)
PMechaSet2=(160.190,-334.970,320.180,122.020,86.640,31.360)(6,0)
PMechaSet2_1=(160.190,-334.970,339.970,122.020,86.640,31.360)(6,0)
PMechaSet_2=(162.580,-305.370,557.380,179.470,90.000,89.470)(6,0)
PMechaSet_3=(114.450,-288.220,565.580,180.000,0.000,112.110)(7,0)
PMechaSet_4=(310.110,-0.040,565.560,180.000,0.000,-179.550)(7,0)
PMechaSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PMechaSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
Pmove=(-20.940,188.680,640.000,-180.000,0.000,96.340,0.000,0.000)(7,0)
PPlateBackCheck=(-90.210,513.030,577.720,-180.000,0.000,-90.000)(7,0)
PPlateBackCheck_2=(66.390,429.860,577.750,180.000,0.000,-90.000)(7,0)
PPlateBackCheck_3=(-18.780,286.220,630.880,180.000,0.000,-90.000)(7,0)
PPlateBackCheck_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackCheck_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackCheck_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet=(477.780,104.880,400.930,179.920,-0.070,179.070)(7,0)
PPlateBackGet_1=(477.780,104.880,430.000,179.920,-0.070,179.070)(7,0)
PPlateBackGet_2=(477.780,104.880,560.000,179.920,-0.070,179.070)(7,0)
PPlateBackGet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPlateBackPush=(-18.720,415.610,534.990,179.960,0.000,88.990)(7,1048576)
PPlateBackPush_1=(-18.720,400.000,534.990,179.960,0.000,88.990)(7,1048576)
PPlateBackPush_2=(-17.580,398.810,550.570,179.960,0.000,88.990)(7,1048576)
PPlateBackSet=(-17.580,456.120,539.500,-179.730,-11.000,88.560)(7,1048576)
PPlateBackSet_1=(-17.580,449.110,537.790,-179.680,-13.000,88.560)(7,1048576)
PPlateBackSet_10=(-17.620,351.380,478.080,-178.780,-45.000,88.590)(7,1048576)
PPlateBackSet_11=(-17.620,346.000,478.080,-178.780,-45.000,88.590)(7,1048576)
PPlateBackSet_12=(-17.620,345.820,487.160,179.690,-45.000,90.690)(7,1048576)
PPlateBackSet_13=(-17.620,286.220,630.900,-179.820,-0.290,90.490)(7,1048576)
PPlateBackSet_2=(-17.580,435.640,533.420,-179.580,-17.000,88.560)(7,1048576)
PPlateBackSet_3=(-17.580,422.800,528.430,-179.470,-21.000,88.560)(7,1048576)
PPlateBackSet_4=(-17.580,409.820,522.120,-179.370,-25.000,88.560)(7,1048576)
PPlateBackSet_5=(-17.580,397.610,515.400,-179.260,-29.000,88.560)(7,1048576)
PPlateBackSet_6=(-17.580,385.280,508.060,-179.140,-33.000,88.560)(7,1048576)
PPlateBackSet_7=(-17.580,373.780,499.740,-179.030,-37.000,88.590)(7,1048576)
PPlateBackSet_8=(-17.580,363.120,490.440,-178.910,-41.000,88.680)(7,1048576)
PPlateBackSet_9=(-17.580,353.110,479.810,-178.780,-45.000,88.590)(7,1048576)
PProductOnPltGet=(477.970,-98.800,372.680,179.760,-0.070,178.770)(7,0)
PProductOnPltGet_1=(477.970,-98.800,410.000,179.760,-0.070,178.770)(7,0)
PProductOnPltGet_2=(477.970,-98.800,500.000,179.760,-0.070,178.770)(7,0)
PProductOnPltGet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet=(476.450,-98.290,372.680,179.790,-0.350,178.820)(7,0)
PProductOnPltSet_1=(476.450,-98.290,410.000,179.790,-0.350,178.820)(7,0)
PProductOnPltSet_2=(476.450,-98.290,500.000,179.790,-0.350,178.820)(7,0)
PProductOnPltSet_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnPltSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboGet=(-18.410,403.360,318.120,76.690,88.680,166.880)(6,0)
PProductOnRoboGet_1=(-18.410,403.360,425.200,76.690,88.680,166.880)(6,0)
PProductOnRoboGet_2=(-18.410,307.970,425.200,76.690,88.680,166.880)(6,0)
PProductOnRoboGet_3=(-18.400,300.000,550.000,75.410,88.780,165.600)(6,0)
PProductOnRoboGet_4=(-18.400,300.000,550.000,75.410,88.780,165.600)(6,0)
PProductOnRoboGet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboGet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboSet=(-18.410,403.360,318.120,76.690,88.680,166.880)(6,0)
PProductOnRoboSet_1=(-18.410,403.360,425.200,76.690,88.680,166.880)(6,0)
PProductOnRoboSet_2=(-18.410,307.970,425.200,76.690,88.680,166.880)(6,0)
PProductOnRoboSet_3=(-18.400,300.000,550.000,75.410,88.780,165.600)(6,0)
PProductOnRoboSet_4=(-18.860,404.690,360.000,-102.740,89.080,-12.360)(6,0)
PProductOnRoboSet_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PProductOnRoboSet_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PPushTilt=(-213.310,564.750,464.130,179.960,-0.020,1.510)(7,0)
PPushTilt_1=(-213.310,564.750,480.780,179.960,-0.020,1.510)(7,0)
PPushTilt_2=(-213.310,564.750,620.000,179.960,-0.020,1.510)(7,0)
PPushTilt_3=(0.020,340.000,610.000,-180.000,-0.010,-91.910)(7,0)
PTemp=(602.000,-150.000,550.000,-180.000,0.000,90.000,0.000,0.000)(7,0)
PTicketRead=(602.000,-150.000,500.000,180.000,0.000,90.000)(7,0)
PTicketRead_1=(602.000,-150.000,550.000,180.000,0.000,90.000)(7,0)
PTicketRead_2=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_3=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_4=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_5=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PTicketRead_6=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)(0,0)
PEscapePosi(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PEscapePosi(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PGetScrewPos(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(1)=(602.000,-150.000,500.000,180.000,0.000,90.000,0.000,0.000)(7,0)
PInspPosition(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(11)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(12)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(13)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(14)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(15)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(16)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(17)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(18)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(19)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(20)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(21)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(22)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(23)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(24)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(25)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(26)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(27)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(28)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(29)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PInspPosition(30)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(1)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(2)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(3)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(4)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(5)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(6)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(7)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(8)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(9)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
PScrewPos(10)=(0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000)
