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
376 '   BaseUnit6�ʐM�m�F CD�ɔ����폜 2022/07/27 M.H
377 '
378 '
379 ' PIAS�`�P�b�g�Ǎ��ݍH�������m�F
380     M_20# = MClear%                       '������
381 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
382 '        MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
383 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
384 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
385 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' ���j���[�֖߂�
386 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NG���H�������ɏ����ݎ��̍H����
387 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NG���H�������ɏ����ݎ��̍H����
388 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' ����NG, �H������
389 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK�ȊO�͑g���I��
390 '    EndIf
391 '    '
392 '    '���W�ړ�
393 '    '
394 '    '����xx��~
395 '    fScewTCaseStop(MScwT_Case5%)
396 '    '
397 '    '�x�[�X���j�b�gKEY
398 '    Wait M_In(MTEST_KEY%) = MOn%
399 '    '
400 '    '�ĊJ�n
401 '    fScewTReStart()
402 '    '
403 '    '���W�ړ�
404 '    '
405 '    '�˂����ߊ���
406 '    Mret% = fScewTFinish()
407 ' �l�W���߃e�X�g�I��
408 ' PIAS�e�X�g -----------
409 '    MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
410 '    MRet% = fnPiasWrite(MNG%)
411  '   MRet% = fnPCBNumberCheck()
412 ' PIAS�e�X�g�I�� -------
413 '
414     '�g���J�n(9/6�ǉ�(����))
415     '�v���O�������_
416     Ovrd 100
417     ' �n���h��ԏ�����(10/29�ǉ�M.H)(2/11�C��(����))
418     Cmp Off                     '�R���v���C�A���X���[�h�I��
419     ColChk On                   '�Փˌ��mON
420     If M_In(11266) Then
421         M_Out(12256) = 0
422         M_Out(12257) = 1
423     EndIf
424     If M_In(11269) Then
425         M_Out(12258) = 0
426         M_Out(12259) = 1
427     EndIf
428     If M_In(11271) Then
429         M_Out(12260) = 0
430         M_Out(12261) = 1
431     EndIf
432     *WAIT_HAND_INI
433     If M_In(11265) = 1 And M_In(11268) = 1 And M_In(11270) = 1 Then GoTo *CompHandIni Else GoTo *WAIT_HAND_INI
434     *CompHandIni
435     M_Out(12257) = 0
436     M_Out(12259) = 0
437     M_Out(12261) = 0
438 '
439 '
440 'Dly 5                           '�f�o�b�O�p(22/09/30����)
441     ' �˂����ߋ@�e�X�g�p ----------
442     Mret% = fScrewTcomChk()
443     If Mret% = -1 Then GoTo *ASSY_ERROR_END
444     '�`�P�b�gID��ǂ�
445     M_Out(12262) = 1 Dly 0.5 '�{�̈ʒu���ߏo�[ON
446     PTemp = P_Curr
447     MRtn = 0
448 '    If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
449 '        If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
450 '            If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
451 '                MRtn = 1
452 '            EndIf
453 '        EndIf
454 '    EndIf
455 '    If MRtn = 1 Then
456 '        Mov PTicketRead
457 '    Else
458 '        Cnt 1 , 10 , 10
459 '        Mov PInitialPosition
460 '        Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
461 '        Cnt 0
462 '        Mvs PTicketRead             'ID�ǂ݈ʒu
463 '    EndIf
464 '
465 ' 2022/04/12 ���S�����֏����ύX �n��
466 ' PInitialPosition �ݐ� MStandby=2
467 ' PTicketRead_1 �ݐ� MStandby=1
468 '
469     MStandby = 0    '�ҋ@�ʒu�t���O��������
470     If (PTemp.X <= PInitialPosition.X + 1.0) And (PTemp.X >= PInitialPosition.X - 1.0) Then
471         If ((PTemp.Y <= PInitialPosition.Y + 1.0) And (PTemp.Y >= PInitialPosition.Y - 1.0)) Then
472             If ((PTemp.Z <= PInitialPosition.Z + 1.0) And (PTemp.Z >= PInitialPosition.Z - 1.0)) Then
473                 MStandby = 2
474             EndIf
475         EndIf
476     EndIf
477     If (PTemp.X <= PTicketRead_1.X + 1.0) And (PTemp.X >= PTicketRead_1.X - 1.0) Then
478         If ((PTemp.Y <= PTicketRead_1.Y + 1.0) And (PTemp.Y >= PTicketRead_1.Y - 1.0)) Then
479             If ((PTemp.Z <= PTicketRead_1.Z + 1.0) And (PTemp.Z >= PTicketRead_1.Z - 1.0)) Then
480                 MStandby = 1
481             EndIf
482         EndIf
483     EndIf
484     If MStandby = 2 Then
485         Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
486         Cnt 0
487     EndIf
488     If MStandby <> 0 Then GoTo *PositionOK
489     fErrorProcess(11,230,281,0)            '�����ʒu�ɂ��Ȃ����̓G���[�ɂ���
490     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
491     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
492     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
493     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
494     *PositionOK
495 '
496     Mvs PTicketRead             'ID�ǂ݈ʒu
497 ' CD�ɔ����폜 2022/07/27 M.H
498 '    M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
499 '    M_Out(12258) = 1            'DVD���J�`���b�N��ON
500 '
501     '
502     MRtn = 1        'MRtn������
503 *RE_TICKET_READ
504 '    MRtn = fnPiasCheck()               'ID�ǂݎ��
505 'PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
506 'MInspGroup%(1) = 1              '����G�ԍ�
507 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
508 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
509     MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
510     '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
511     '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
512 EndIf
513 If MRtn = 1 Then GoTo *CompRead
514 '
515     '�G���[�����i�ʒu���߂�����
516 *RE_ERR_REL_1
517 If M_20# = MContinue% Then M_20# = MRtn
518 M_Out(12263) = 1 Dly 0.5    '���i�ʒu���߂�����
519 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)
520 '
521 If MRtn = 1 Then GoTo *CompErrorRelease
522 MRtn = M_20#        'M_20#�ꎞ���
523 M_20# = MClear%
524 fErrorProcess(11,234,284,0)     '�ʒu���ߖߒ[�G���[
525 If M_20# = MContinue% Then GoTo *RE_ERR_REL_1
526 If M_20# = MNext% Then M_20# = MRtn
527 If M_20# = MNgProcess% Then M_20# = MAbout%
528 *CompErrorRelease
529 '
530 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
531 If M_20# = MNext% Then M_20# = MPass%
532 Mvs PTicketRead_1                         '22/04/07 �ǉ� �n��
533 GoTo *ASSY_ERROR_END
534 *CompRead
535     fScrewTStart()           '�����ʒu�ύX2/27����)
536 '
537     '�p���b�g���琻�i�����
538     '
539     *RE_POSITIONING
540     '
541     M_Out(12262) = 1 Dly 0.5 '�{�̈ʒu���ߏo�[ON
542 '    Wait M_In(11273) = 1     '�{�̈ʒu���ߏo�[���o
543     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '�{�̈ʒu���ߏo�[���o
544     If MRtn = 1 Then GoTo *CompPositioning
545     fErrorProcess(11,231,282,0)
546     If M_20# = MNext% Then M_20# = MClear%
547     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
548     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
549     If M_20# = MContinue% Then GoTo *RE_POSITIONING
550     *CompPositioning
551 '
552     Mov PProductOnPltGet_2      '�{�̎󂯎������_
553 '
554 ''    Wait M_In(11888) = 1        '�˂����{2��~1��M(�����ʒu�ύX3/14����)
555 '    MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
556 '    *RE_ERR_REL_2
557 '    If M_20# = MContinue% Then M_20# = MRtn2
558 '    If MRtn = 0 Then
559 '        MRtn2 = 1       'MRtn2������
560 '        M_Out(12263) = 1 Dly 0.5    '���i�ʒu���߂�����
561 '        Mov PInitialPosition  '"�C�j�V�����ɖ߂铮��"
562 '        MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
563 '        If MRtn2 = 0 Then
564 '            MRtn2 = M_20#                   '�ʒu���ߖߒ[�G���[�Ȃ�M_20#�������ꎞ���
565 '            M_20# = MClear%                 'M_20#������
566 '            fErrorProcess(11,234,284,0)     '�ʒu���ߖߒ[�G���[
567 '            If M_20# = MNext% Then M_20# = MRtn2        'M_20#�ɔ����l����
568 '                '�ʒu���߃G���[���������čH���𔲂���ꍇ��~�������s��
569 '            If M_20# = MNgProcess% Then M_20# = MAbout%
570 '            Break
571 '        EndIf
572 '        Break
573 '            EndIf
574 '    If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
575 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
576 '
577 '    Mov PProductOnPltGet_1      '�{�̎󂯎����
578     '
579     *RE_PLT_GET_1
580     '
581     M_Out(12256) = 0            '�{�̃`���b�N��OFF
582     M_Out(12257) = 1            '�{�̃`���b�N�JON
583     '
584 '    Wait M_In(11265) = 1        '�{�̃`���b�N�J���o
585     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
586     If MRtn = 1 Then GoTo *CompPltGet1
587     fErrorProcess(11,244,284,0)
588     If M_20# = MNext% Then M_20# = MClear%
589     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
590     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
591     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
592     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
593     *CompPltGet1
594     '
595     Mov PProductOnPltGet_1      '�{�̎󂯎����
596     '
597     Ovrd 25
598 '    Fine 0.05 , P
599     Mvs PProductOnPltGet        '�{�̎󂯎��ʒu
600     Dly 0.1
601     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
602     M_Out(12256) = 1            '�{�̃`���b�N��ON
603 '    Fine 0 , P
604     '
605     M_Out(12263) = 1 Dly 0.5                    '�{�̈ʒu���ߖߒ[ON
606 '    Wait M_In(11274) = 1     '�{�̈ʒu���ߖߒ[���o
607     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)    '�{�̈ʒu���ߖߒ[���o
608     If MRtn = 1 Then GoTo *CompPltGet2
609     M_Out(12256) = 0                            '�{�̃`���b�N��OFF
610     M_Out(12257) = 1                            '�{�̃`���b�N�JON
611     Dly 2.0
612     Mvs PProductOnPltGet_1
613     Mov PProductOnPltGet_2
614     fErrorProcess(11,234,284,0)
615     If M_20# = MNext% Then M_20# = MClear%
616     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
617     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
618     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
619     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
620     Mov PProductOnPltGet_1
621     Mvs PProductOnPltGet
622     M_Out(12257) = 0                            '�{�̃`���b�N�JOFF
623     M_Out(12256) = 1                            '�{�̃`���b�N��ON
624     Dly 2.0
625     *CompPltGet2
626     '
627 '    Wait M_In(11264) = 1        '�{�̌��o
628     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '�{�̌��o
629     If MRtn = 1 Then GoTo *CompPltGet3
630     M_Out(12256) = 0            '�{�̃`���b�N��OFF
631     M_Out(12257) = 1            '�{�̃`���b�N�JON
632     Dly 2.0
633     Mvs PProductOnPltGet_1
634     Mov PProductOnPltGet_2
635     fErrorProcess(11,252,284,0)
636     If M_20# = MNext% Then M_20# = MClear%
637     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
638     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
639     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
640     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
641     Mov PProductOnPltGet_1
642     Mvs PProductOnPltGet
643     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
644     M_Out(12256) = 1            '�{�̃`���b�N��ON
645     Dly 2.0
646     *CompPltGet3
647     '
648 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
649     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
650     If MRtn = 1 Then GoTo *CompPltGet4
651     M_Out(12256) = 0            '�{�̃`���b�N��OFF
652     M_Out(12257) = 1            '�{�̃`���b�N�JON
653     Dly 2.0
654     Mvs PProductOnPltGet_1
655     Mov PProductOnPltGet_2
656     Dly 0.1
657     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
658     M_Out(12256) = 1            '�{�̃`���b�N��ON
659     Dly 3.0
660     fErrorProcess(11,245,284,0)
661     If M_20# = MNext% Then M_20# = MClear%
662     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
663     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
664     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
665     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
666     M_Out(12256) = 0            '�{�̃`���b�N��OFF
667     M_Out(12257) = 1            '�{�̃`���b�N�JON
668     Dly 2.0
669     Mov PProductOnPltGet_1
670     Mvs PProductOnPltGet
671     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
672     M_Out(12256) = 1            '�{�̃`���b�N��ON
673     Dly 2.0
674     *CompPltGet4
675     '
676     Dly 0.2                     '�O�̂��߃f�B���C
677     Cnt 1 , 10 , 10
678     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
679     Mvs PProductOnPltGet_1      '�{�̎󂯎����
680     MRtn = FnCtlValue2(99)       '�Ǐ��J�n�M��OFF  2022/04/28 �n��
681     Ovrd 100
682     Mov PProductOnPltGet_2      '�{�̎󂯎������_
683     '
684     '���i���˂����{2�ɒu��
685     Mov PProductOnRoboSet_3     '�o�H
686     Cnt 0
687 '    Wait M_In(11888) = 1        '�˂����{2��~1��M(�����ʒu,�����ύX3/1����)
688     MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
689     *RE_ERR_REL_2
690     If MRtn = 0 Then
691         Cnt 0
692         Mov PProductOnPltSet_2
693         Mov PProductOnPltSet_1
694         Mvs PProductOnPltSet
695         M_Out(12256) = 0        '�{�̃`���b�N��OFF
696         M_Out(12257) = 1        '�{�̃`���b�N�JON
697         Dly 2.0
698         Mvs PProductOnPltSet_1
699         Mvs PProductOnPltSet_2
700         Mov PInitialPosition
701     EndIf
702     If MRtn = 0 Then GoTo *ASSY_ERROR_END
703     '
704     *RE_ROBO_SET_1
705     '
706     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
707     M_Out(12258) = 1            'DVD���J�`���b�N��ON
708 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
709     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
710     If MRtn = 1 Then GoTo *CompRoboSet1
711     fErrorProcess(11,269,284,0)
712     If M_20# = MNext% Then M_20# = MClear%
713     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
714     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
715     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
716     *CompRoboSet1
717 '
718     Mvs PProductOnRoboSet_2     '�˂����{���i�u�������_
719 '    Mvs PProductOnRoboSet_4     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)
720     Ovrd 25
721     Mvs PProductOnRoboSet_1     '�˂����{���i�u�����
722     Mvs PProductOnRoboSet       '�˂����{���i�u���ʒu
723     M_Out(12866) = 1 Dly 0.3    '�˂����{2����ĊJ(��~1�`��~2)
724 '    Wait M_In(11889) = 1        '�˂����{2��~2��M
725     MScrewRoboNgFlg% = 0
726     MRtn = fScrewTighenRoboCheck(11889)    '��~��Ԃ���M����
727     If MRtn = 0 Then
728         MScrewRoboNgFlg% = 1
729     EndIf
730 '
731     *RE_ROBO_SET_2
732 '
733     M_Out(12256) = 0            '�{�̃`���b�N��OFF
734     M_Out(12257) = 1            '�{�̃`���b�N�JON
735 '    Wait M_In(11265)            '�{�̃`���b�N�J���o
736     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
737     If MRtn = 1 Then GoTo *CompRoboSet2
738     fErrorProcess(11,244,284,0)
739     If M_20# = MNext% Then M_20# = MClear%
740     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
741     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
742     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_2
743     *CompRoboSet2
744     '
745     Mvs PProductOnRoboSet_1     '�˂����{���i�u�����
746     Mvs PProductOnRoboSet_2     '�˂����{���i�u�������_
747 '    Mvs PProductOnRoboSet_4     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)
748     Ovrd 100
749     Cnt 1 , 10 , 10
750     Mov PProductOnRoboSet_3     '�o�H
751     '
752     If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition
753     If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
754 '
755 '
756 '
757     '
758     '�`���g�X���C�_�[������
759     Cnt 1 , 10
760     Mov PPushTilt_3             '�`���g�X���C�_�[���������ς����_
761     Cnt 0
762     Mov PPushTilt_2             '�`���g�X���C�_�[�������_
763     Ovrd 30
764     Mvs PPushTilt_1             '�`���g�X���C�_�[�������
765     Spd 1000
766     Ovrd 5
767     Mvs PPushTilt               '�`���g�X���C�_�[����
768     Spd M_NSpd
769     Ovrd 30
770     Cnt 1 , 1 , 1
771     Mvs PPushTilt_1             '�`���g�X���C�_�[�������
772     Cnt 1 , 1 , 10
773     Ovrd 100
774     Mov PPushTilt_2             '�`���g�X���C�_�[�������_
775     '
776     '�w�ʔ����(�R���v���C�A���X���[�h����11/8����)
777     Mov PPlateBackGet_2         '�w�ʔ󂯎������_
778 '    Cnt 1 , 10'�b��
779     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~2�`��~3)
780 '    MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����'12/20�ʒu�̕ύX(����)
781 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
782 '    Mov PPlateBackGet_1         '�w�ʔ󂯎����'�b��
783     Cnt 0
784     '
785     *RE_PLATE_GET
786     '
787     Fine 0.05 , P               '�t�@�C������ON
788     Ovrd 25
789     Mvs PPlateBackGet           '�w�ʔ󂯎��ʒu
790 '    Dly 0.2                     '�ꎞ�R�����g�A�E�g
791     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
792     M_Out(12256) = 1            '�{�̃`���b�N��ON
793     '
794 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
795     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
796     If MRtn = 1 Then GoTo *CompPlateGet_1
797     M_Out(12256) = 0            '�{�̃`���b�N��OFF
798     M_Out(12257) = 1            '�{�̃`���b�N�JON
799     Mvs PPlateBackGet_1
800     fErrorProcess(11,245,293,0) '284��293�ɕύX6/2����
801     If M_20# = MNext% Then M_20# = MClear%
802     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
803     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
804     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
805     Mvs PPlateBackGet           '�w�ʔ󂯎��ʒu
806     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
807     M_Out(12256) = 1            '�{�̃`���b�N��ON
808     *CompPlateGet_1
809     Fine 0 , P                  '�t�@�C������OFF
810     '
811     Ovrd 5
812     Accel 25 , 100
813     Dly 0.7                     '�f�B���C���Ԓ��ߒ�(�c���͊m��)
814 '    CmpG 0.7,0.7,,,,,,       'X,Y���Q�C����0.7�ɕύX
815 '    ColChk Off                  '�Փˌ��mOFF
816 '    Cmp Pos , &B11          'X,Y���R���v���C�A���X���[�h�J�n
817     Mov PPlateBackGet_1         '�w�ʔ󂯎����
818     Cnt 1 , 10 , 10
819 '    Cmp Off                     '�R���v���C�A���X���[�h�I��
820 '    ColChk On                   '�Փˌ��mON
821     Ovrd 50
822     Mov PPlateBackGet_2         '�w�ʔ󂯎������_
823     Ovrd 100
824     Accel 100 , 100
825     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '�w�ʃp�l���`�F�b�N
826     If MRtn = 1 Then GoTo *CompPlateGet_2
827     Cnt 0
828     fErrorProcess(11,267,293,0)                     '284��293�ɕύX6/2����
829     If M_20# = MNext% Then M_20# = MClear%
830     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
831     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
832     If M_20# = MContinue% Then
833         Mov PPlateBackGet_1
834         Dly 0.3
835         M_Out(12256) = 0            '�{�̃`���b�N��OFF
836         M_Out(12257) = 1            '�{�̃`���b�N�JON
837         Dly 2.0
838     EndIf
839     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
840     *CompPlateGet_2    '
841     '�w�ʔ�u��
842 '    Wait M_In(11889) = 1        '�˂����{2��~2��M
843     ColChk Off
844     Mov PPlateBackSet_12        '�w�ʔu�����
845     Cnt 1 , 10
846 '
847     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '������x�w�ʃp�l���`�F�b�N
848     If MRtn = 1 Then GoTo *CompPlateGet_3
849     fErrorProcess(11,267,293,0)                     '284��293�ɕύX6/2����
850     If M_20# = MNext% Then M_20# = MClear%
851     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
852     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
853     If M_20# = MContinue% Then
854         Mov PPlateBackGet_2
855         Mov PPlateBackGet_1
856         M_Out(12256) = 0            '�{�̃`���b�N��OFF
857         M_Out(12257) = 1            '�{�̃`���b�N�JON
858         Dly 2.0
859     EndIf
860     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
861     *CompPlateGet_3
862 '
863 ' CD�ɔ����폜 2022/07/27 M.H
864 '    ' ���i�����v�����M
865 '    M_Out(12787) = 1
866 '
867     MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����'12/20�ʒu�̕ύX(����)
868 '    If MRtn = 0 Then Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
869     If MRtn = 0 Then GoTo *ASSY_ERROR_END
870 '
871 '    Mov PPlateBackSet_11        '�o�H1
872     Ovrd 50
873 '    Mov PPlateBackSet_10        '�o�H2
874 '    Mov PPlateBackSet_9         '�o�H3
875 '    Mov PPlateBackSet_8         '�o�H4
876 '    Mov PPlateBackSet_7         '�o�H5
877     Mov PPlateBackSet_6         '�o�H6
878     Cnt 0
879     Ovrd 25
880     Mvs PPlateBackSet_5         '�o�H7
881     Mvs PPlateBackSet_4         '�o�H8
882     Mov PPlateBackSet_3         '�o�H9
883     Mov PPlateBackSet_2         '�o�H10
884     Mov PPlateBackSet_1         '�o�H11
885     Mov PPlateBackSet           '�w�ʔu���ʒu
886     *RE_PLATE_SET
887     M_Out(12256) = 0            '�{�̃`���b�N��OFF
888     M_Out(12257) = 1            '�{�̃`���b�N�JON
889     '
890 '    Wait M_In(11265)            '�{�̃`���b�N�J���o
891     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
892     If MRtn = 1 Then GoTo *CompPlateSet
893     fErrorProcess(11,244,284,0)
894     If M_20# = MNext% Then M_20# = MClear%
895     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
896     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
897     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
898     *CompPlateSet
899     '
900     ColChk On
901     Mov PPlateBackSet_12        '�w�ʔu�����
902     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`��~4)
903     Ovrd 100
904     '
905 ''    ' ���i�����v�����M(�����ʒu�ύX2/27����)
906 '    M_Out(12787) = 1
907     '�˂����{���i�N�����v�Œ�҂�
908 '    Wait M_In(11891) = 1        '�˂����{2��~4��M
909 MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
910 'If MRtn = 0 Then Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
911 If MRtn = 0 Then GoTo *ASSY_ERROR_END
912     '�u���ʒu�摜����
913 '    Mov PPlateBackCheck_3       '�摜�����ʒu���
914 '    Mov PPlateBackCheck_2       '�ʉߓ_
915 '    Mvs PPlateBackCheck         '�m�F�ʒu
916     '
917     'PInspPosition(2) = PPlateBackCheck
918     'MInspGroup(2) = 2
919     'MRtn = ISInspection(PInspPosition,MInspGroup,2,-1,1)
920     'If MRtn <> 1 Then
921     '   '�G���[����
922     'EndIf
923     '
924 ''    ' ���i�����v�����M
925 '    M_Out(12787) = 1    '�����ʒu�ύX
926 '    Mov PPlateBackCheck_3       '�摜�����ʒu���
927     '
928     '�˂����{�������ݑ҂�
929     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`����)
930     '
931     'DVD���J����� CD�ɔ����폜 2022/07/27 M.H
932 *Loop_CW_CCW_S
933     *RE_MECHA_SET_1
934 MRtn = fScrewTighenRoboCheck(11876)    '��~��Ԃ���M����
935 'If MRtn = 0 Then Mov PInitialPosition   '"�C�j�V�����ɖ߂铮��"
936 If MRtn = 0 Then GoTo *ASSY_ERROR_END
937 '
938     *CompRoboGet1
939     '
940 '    Ovrd 50
941     Mov PProductOnRoboGet_3     '�˂����{���i�������_2����3��
942 '    Ovrd 20
943     Mvs PProductOnRoboGet_2     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����2��
944     Ovrd 20
945     Mvs PProductOnRoboGet_1     '�˂����{���i�����
946     Ovrd 10
947     Mvs PProductOnRoboGet       '�˂����{���i���ʒu
948     Dly 0.2
949 '
950     *RE_ROBO_GET_2
951 '
952     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
953     M_Out(12256) = 1            '�{�̃`���b�N��ON
954 '
955 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
956     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
957     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
958     M_Out(12256) = 0            '�{�̃`���b�N��OFF
959     M_Out(12257) = 1            '�{�̃`���b�N�JON
960     Dly 2.0
961     Mvs PProductOnRoboGet_1
962     Mvs PProductOnRoboGet_2
963     Mov PProductOnRoboGet_3
964     Mov PProductOnRoboGet_4
965     Mov PInitialPosition
966     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
967     M_Out(12256) = 1            '�{�̃`���b�N��ON
968     Dly 1.0
969     fErrorProcess(11,245,284,0)
970     If M_20# = MNext% Then MRtn = 1
971     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
972     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
973     M_Out(12256) = 0            '�{�̃`���b�N��OFF
974     M_Out(12257) = 1            '�{�̃`���b�N�JON
975     Dly 2.0
976     Mov PProductOnRoboGet_4
977     Mov PProductOnRoboGet_3
978     Mov PProductOnRoboGet_2
979     Mvs PProductOnRoboGet_1
980     Mvs PProductOnRoboGet
981     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
982     *CompRoboGet2
983     M_20# = MClear%
984     '
985     Dly 0.2
986     Mvs PProductOnRoboGet_1     '�˂����{���i�����
987     Ovrd 50
988     Mvs PProductOnRoboGet_2     '�˂����{���i�������_(9/27�b��R�����g�A�E�g)12/15�R�����g����
989     Mvs PProductOnRoboGet_3     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����3��
990     Ovrd 100
991     Mov PProductOnRoboGet_4     '�o�H3����4��
992     Cnt 1 , 10 , 10
993 '
994     M_Out(12868) = 1 Dly 0.3    '�˂����{2���슮���𑗐M
995     *RE_ROBO_GET_3
996 ' CD�ɔ����폜 2022/07/27 M.H
997 '    M_Out(12258) = 0            'DVD���J�`���b�N��OFF
998 '    M_Out(12259) = 1            'DVD���J�`���b�N�JON
999 ''    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1000 '    MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1001 '    If MRtn = 1 Then GoTo *CompRoboGet3
1002 '    fErrorProcess(11,270,284,0)
1003 '    If M_20# = MNext% Then M_20# = MClear%
1004 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1005 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1006 '    If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1007     *CompRoboGet3
1008     '
1009     '�p���b�g�֐��i��u��
1010     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1011     Cnt 1 , 10
1012     Mov PProductOnPltSet_1      '�{�̒u���ʒu���
1013     Cnt 0
1014     Ovrd 10
1015     Mvs PProductOnPltSet        '�{�̒u���ʒu
1016     Dly 0.5
1017 '
1018     *RE_PLT_SET
1019 '
1020     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1021     M_Out(12257) = 1            '�{�̃`���b�N�JON
1022 '
1023     Wait M_In(11265) = 1        '�{�̃`���b�N�J���o
1024 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1025     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1026     If MRtn = 1 Then GoTo *CompPltSet
1027     fErrorProcess(11,244,284,0)
1028     If M_20# = MNext% Then M_20# = MClear%
1029     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1030         Mvs PProductOnPltSet_1
1031         Mov PProductOnPltSet_2
1032         Mov PInitialPosition
1033     EndIf
1034     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1035     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1036     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1037     *CompPltSet
1038 '
1039     Mvs PProductOnPltSet_1      '�{�̒u���ʒu���
1040     Ovrd 100
1041     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1042 '    Mov PInitialPosition        '�C�j�V�����|�W�V����
1043     MRtn = FnCtlValue2(2)          '�g���n�j�{�P  2022/04/28 �n��
1044     Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
1045     MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1046     '
1047     '�`�P�b�gID��������
1048     M_20# = MAssyOK%
1049     *ASSY_ERROR_END
1050     *AssyEnd
1051     *fnAssyStart_FEndPosi
1052 FEnd
1053 '
1054 '��fnPiasCheck
1055 ''' <summary>
1056 ''' PIAS�`�P�b�g�Ǎ���
1057 ''' </summary>
1058 ''' <returns>   0 : NG
1059 '''             1 : OK(�Ǎ��݊���)
1060 ''' </returns>
1061 ''' <remarks>
1062 ''' Date   : 2021/07/07 : M.Hayakawa
1063 ''' </remarks>'
1064 Function M% fnPiasCheck
1065     fnPiasCheck = 0
1066     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1067     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1068 '
1069 *RETRY_PIAS
1070     M_20# = MClear%
1071     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1072     '
1073     '�yID�`�P�b�g�ǂݍ��݁z
1074     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1075     MInspGroup%(1) = 1              '����G�ԍ�
1076     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1077 '
1078     '�G���[�̏ꍇ
1079     If MRtn <> 1 Then
1080         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1081         If MRtn <> 1 Then
1082             'D720 -> D1300 �R�s�[�v��
1083             M_Out(12565) = 1
1084             Dly 0.5
1085             M_Out(12565) = 0
1086             '�G���[�����L�q
1087             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1088             'GOT KEY���͑҂�
1089             MKeyNumber = fnKEY_WAIT()
1090             '
1091             Select MKeyNumber
1092                 Case MNext%         '���ւ�I�������ꍇ
1093                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1094                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1095                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1096                     Break
1097                 Case MAbout%        '��~��I�������ꍇ
1098                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1099                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1100                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1101                     Break
1102                 Case MNgProcess%    'NG��I�������ꍇ
1103                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1104                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1105                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1106                     Break
1107                 Case MContinue%     '�p����I�������ꍇ
1108                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1109                     M_20# = MContinue%
1110                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1111                     Break
1112             End Select
1113         EndIf
1114     EndIf
1115 '----------D720 -> D1300 �R�s�[�v��----------
1116     M_Out(12565) = 1
1117     Dly 0.5
1118     M_Out(12565) = 0
1119 '----------�ʐM�m�F������----------
1120     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1121     MRtn = 0                ' ������
1122     M_20# = MClear%         ' ������
1123     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1124     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1125     If MRtn <> 1 Then
1126         If M_20# = MContinue% Then
1127             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1128         Else
1129             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1130         EndIf
1131     EndIf
1132 '----------�H�������m�F----------
1133     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1134     MRtn = 0                ' ������
1135     M_20# = MClear%         ' ������
1136     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1137     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1138     If MRtn <> 1 Then
1139         If M_20# = MContinue% Then
1140             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1141         Else
1142             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1143         EndIf
1144     EndIf
1145     '
1146     fnPiasCheck = 1
1147     *fnPiasCheck_End
1148 FEnd
1149 '
1150 '��fnPCComuCheck
1151 ''' <summary>
1152 ''' PC-PLC�ʐM�`�F�b�N
1153 ''' </summary>
1154 ''' <returns>   0 : NG
1155 '''             1 : OK(�Ǎ��݊���)
1156 ''' </returns>
1157 ''' <remarks>
1158 ''' Date   : 2021/07/07 : M.Hayakawa
1159 ''' </remarks>'
1160 Function M% fnPCComuCheck
1161     fnPCComuCheck = 0
1162     MJudge% = 0                                  '������
1163     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1164     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1165     '
1166     For MStaNo = 0 To 5
1167         '
1168         If M_In(MIN_PIAS_ComOK%) = 1 Then
1169             'PC�ʐMOK(M400)
1170             MJudge% = MOK%
1171             MStaNo = 5
1172             Break
1173         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1174             'toRBT_�ʐM�m�Ftime out
1175             MJudge% = MNG%
1176             MCommentD1001 = 15
1177             MCommentD1002 = 21
1178             MStaNo = 5
1179             Break
1180         Else
1181             'toRBT_�ʐM�m�Ftime out
1182             MJudge% = MNG%
1183             MCommentD1001 = 14
1184             MCommentD1002 = 21
1185             Break
1186         EndIf
1187     Next MStaNo
1188     '
1189     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1190     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1191     '
1192     '�G���[���
1193     If MJudge% <> MOK% Then
1194         M_20# = MClear%     '������
1195         '�G���[�����L�q
1196         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1197         'GOT KEY���͑҂�
1198         MKeyNumber = fnKEY_WAIT()
1199         '
1200         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1201             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1202             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1203             Break
1204         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1205             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1206             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1207             Break
1208         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1209             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1210             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1211             Break
1212         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1213             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1214             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1215             Break
1216         EndIf
1217     Else
1218         'OK�̏ꍇ
1219         fnPCComuCheck = 1
1220     EndIf
1221 FEnd
1222 '
1223 '��fnProcessCheck
1224 ''' <summary>
1225 ''' �H�������m�F
1226 ''' </summary>
1227 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1228 '''             -1�F�O�H������NG  -2�F���H����������
1229 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1230 '''             -5�F���������G���[
1231 ''' </returns>
1232 ''' <remarks>
1233 ''' Date   : 2021/07/07 : M.Hayakawa
1234 ''' </remarks>'
1235 Function M% fnProcessCheck
1236     fnProcessCheck = 0
1237     MJudge% = MNG%      '��UNG���������Ƃ���
1238 '----------�H�������m�F----------
1239     MCommentD1001 = 0   '�R�����g������
1240     For MStaNo = 0 To 5
1241         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1242         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1243         '
1244         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1245             MJudge% = MOK%
1246             fnAutoScreenComment(85)     ' AUTO���
1247             MStaNo = 5
1248             Break
1249         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1250             MFlgLoop% = 0
1251             MJudge% = MNG%
1252             MCommentD1001 = 27
1253             MCommentD1002 = 22
1254             fnAutoScreenComment(94)     ' AUTO���
1255             fnProcessCheck = -2         ' NG��-2��Ԃ�
1256             MStaNo = 5
1257             Break
1258         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1259            MJudge% = MNG%
1260             MCommentD1001 = 31
1261             MCommentD1002 = 22
1262             fnAutoScreenComment(83)     ' AUTO���
1263             fnProcessCheck = -3         ' NG��-3��Ԃ�
1264             MStaNo = 5
1265             Break
1266         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1267             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1268             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1269             MJudge% = MNG%
1270             MCommentD1001 = 32
1271             MCommentD1002 = 22
1272             fnAutoScreenComment(84)     ' AUTO���
1273             fnProcessCheck = -1         ' NG��-1��Ԃ�
1274             Dly 1.0
1275             '�H�������m�FOFF
1276             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1277             Dly 1.0
1278            'MStaNo = 5
1279             Break
1280         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1281             MFlgLoop% = 0
1282             MJudge% = MNG%
1283             MCommentD1001 = 29
1284             MCommentD1002 = 22
1285             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1286             fnProcessCheck = -5         ' NG��-5��Ԃ�
1287             MStaNo = 5
1288             Break
1289         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1290             MJudge% = MNG%
1291             If MCommentD1001 = 32 Then
1292                 '�������Ȃ�
1293             Else
1294                 MCommentD1001 = 26
1295             EndIf
1296             MCommentD1002 = 22
1297             fnProcessCheck = -4         ' NG��-4��Ԃ�
1298             MStaNo = 5
1299             Break
1300         Else
1301             MJudge% = MNG%
1302             MCommentD1001 = 28
1303             MCommentD1002 = 22
1304         EndIf
1305     Next MStaNo
1306     '�H�������m�FOFF
1307     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1308     '�ʉߗ���NG �H�������̏ꍇ
1309     If MJudge% = MPass% Then
1310         M_20# = MPass%
1311     EndIf
1312     '
1313     '�G���[���
1314     If MJudge% <> MOK% Then
1315         M_20# = MClear%     '������
1316         '�G���[�����L�q
1317         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1318         'GOT KEY���͑҂�
1319         MKeyNumber = fnKEY_WAIT()
1320         '
1321         Select MKeyNumber
1322             Case MAbout%        '��~��I�������ꍇ
1323                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1324                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1325                 Break
1326             Case MNext%         '���ւ�I�������ꍇ
1327                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1328                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1329                 Break
1330             Case MContinue%     '�p����I�������ꍇ
1331                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1332                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1333                 Break
1334             Case MNgProcess%    'NG��I�������ꍇ
1335                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1336                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1337                 Break
1338         End Select
1339     Else
1340         fnProcessCheck = 1  ' OK��1��Ԃ�
1341     EndIf
1342 FEnd
1343 '
1344 '��fnPiasWrite
1345 ''' <summary>
1346 ''' Pias �g�����ʏ����ݗv��
1347 ''' </summary>
1348 '''<param name="MFlg%">
1349 '''                 MOK%(1) = �H��������OK��������
1350 '''                 MNG%(0) = �H��������NG��������
1351 '''</param>
1352 '''<returns></returns>
1353 ''' <remarks>
1354 ''' Date   : 2021/07/07 : M.Hayakawa
1355 ''' </remarks>'
1356 Function M% fnPiasWrite(ByVal MFlg%)
1357       fnPiasWrite = 0
1358 *RETRY_PIASWRITE
1359     '
1360     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1361    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1362     If MFlg% = MOK% Then
1363         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1364     Else
1365         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1366     EndIf
1367     Dly 0.1                  '�O�̂���
1368     '
1369     'Pias�֏����݊J�n M305 -> ON
1370     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1371     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1372     '
1373     MJudge% = MNG%
1374     '
1375     For MStaNo = 0 To 5
1376         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1377             MJudge% = MOK%
1378             'MRet = fnAutoScreenComment(85)  'AUTO���
1379             MStaNo = 5
1380             Break
1381         '
1382         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1383             MJudge% = MNG%
1384             'MRet = fnAutoScreenComment(85)  'AUTO���
1385            MCommentD1001 = 34
1386            MCommentD1002 = 25
1387             MStaNo = 5
1388             Break
1389         '
1390         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1391             MJudge% = MNG%
1392             'MRet = fnAutoScreenComment(85)  'AUTO���
1393            MCommentD1001 = 35
1394            MCommentD1002 = 25
1395             MStaNo = 5
1396             Break
1397         '
1398         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1399             MJudge% = MNG%
1400             'MRet = fnAutoScreenComment(85)  'AUTO���
1401            MCommentD1001 = 36
1402            MCommentD1002 = 25
1403             MStaNo = 5
1404             Break
1405         '
1406         Else
1407             MJudge% = MNG%
1408            MCommentD1001 = 42
1409            MCommentD1002 = 25
1410         '
1411         EndIf
1412         '
1413     Next MStaNo
1414     '
1415     'Pias�֏����݊J�n M305 -> OfF
1416     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1417     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1418     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1419     '
1420     '
1421     '�ʉߗ���NG �H�������̏ꍇ
1422     If MJudge% = MPass% Then
1423         M_20# = MPass%
1424     EndIf
1425     '
1426    M_20# = MClear%     '������
1427     '
1428     '�G���[���
1429     If MJudge% < MOK% Then
1430     '
1431 '�c���Ă���������ł͎g�p���Ȃ����x��
1432 *RETRY_ERR_WRITE
1433         M_20# = MClear%     '������
1434         '�G���[�����L�q
1435         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1436         'GOT KEY���͑҂�
1437         MKeyNumber = fnKEY_WAIT()
1438         '
1439         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1440             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1441            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1442             Break
1443         '
1444         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1445             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1446             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1447         '
1448         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1449             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1450             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1451         '
1452         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1453             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1454            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1455             Break
1456         '
1457         EndIf
1458         '
1459         If M_20# = MClear% Then *RETRY_ERR_WRITE
1460         '
1461     EndIf
1462     '
1463     If M_20# = MContinue% Then *RETRY_PIASWRITE
1464     '
1465     fnPiasWrite = 1
1466     '
1467 FEnd
1468 '
1469 '��fnPCBNumberCheck
1470 ''' <summary>
1471 ''' Pias ��ԍ��ƍ��v��
1472 ''' </summary>
1473 '''<param name="%"></param>
1474 '''<param name="%"></param>
1475 '''<returns></returns>
1476 ''' <remarks>
1477 ''' Date   : 2021/07/07 : M.Hayakawa
1478 ''' </remarks>'
1479 Function M% fnPCBNumberCheck
1480       fnPCBNumberCheck = 0
1481     '
1482 *RETRY_PCBCHECK
1483     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1484     'Pias�֊�ƍ��J�n M310 -> ON
1485     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1486     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1487     '
1488     MJudge% = MNG%
1489     '
1490     For MStaNo = 0 To 5
1491         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1492             MJudge% = MOK%
1493             fnAutoScreenComment(96)  'AUTO���
1494             MStaNo = 5
1495             Break
1496         '
1497         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1498             MJudge% = MNG%
1499             fnAutoScreenComment(97)  'AUTO���
1500             MCommentD1001 = 37
1501             MCommentD1002 = 25
1502             MStaNo = 5
1503             Break
1504         '
1505         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1506             MJudge% = MNG%
1507             fnAutoScreenComment(98)  'AUTO���
1508             MCommentD1001 = 38
1509             MCommentD1002 = 25
1510             MStaNo = 5
1511             Break
1512         '
1513         ElseIf M_In(11580) = 1 Then                         'time out
1514             MJudge% = MNG%
1515             fnAutoScreenComment(99)  'AUTO���
1516             MCommentD1001 = 39
1517             MCommentD1002 = 25
1518             MStaNo = 5
1519             Break
1520         '
1521         Else
1522             MJudge% = MNG%
1523            MCommentD1001 = 41
1524            MCommentD1002 = 25
1525         '
1526         EndIf
1527         '
1528     Next MStaNo
1529     '
1530     'Pias�֊�ƍ��J�n M310 -> OfF
1531     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1532     '
1533     '
1534     '�ʉߗ���NG �H�������̏ꍇ
1535     If MJudge% = MPass% Then
1536         M_20# = MPass%
1537     EndIf
1538     '
1539    M_20# = MClear%     '������
1540     '
1541     '�G���[���
1542     If MJudge% < MOK% Then
1543     '
1544 '�c���Ă���������ł͎g�p���Ȃ����x��
1545 *RETRY_ERR_PCBNUMBER
1546         M_20# = MClear%     '������
1547         '�G���[�����L�q
1548         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1549         'GOT KEY���͑҂�
1550         MKeyNumber = fnKEY_WAIT()
1551         '
1552         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1553             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1554             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1555             Break
1556         '
1557         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1558             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1559             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1560         '
1561         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1562             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1563             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1564         '
1565         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1566             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1567             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1568             Break
1569         '
1570         EndIf
1571         '
1572         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1573         '
1574     EndIf
1575     '
1576     If M_20# = MContinue% Then *RETRY_PCBCHECK
1577 FEnd
1578 '
1579 '��ScrewTight_S2
1580 ''' <summary>
1581 ''' �˂����߂��s��
1582 ''' </summary>
1583 '''<param name="PScrewPos()">
1584 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1585 '''             PScrewPos(2)    �F�˂����߉��_
1586 '''             PScrewPos(10)   �F�˂����ߏI������
1587 '''</param>
1588 '''<returns>����
1589 '''         0=�ُ�I���A1=����I��
1590 '''</returns>
1591 ''' <remarks>
1592 ''' Date   : 2021/07/07 : M.Hayakawa
1593 ''' </remarks>'
1594 Function M% ScrewTight_S2(ByVal PScrewPosition())   '�l�W���ߌʐݒ�
1595     ScrewTight_S2 = 0
1596     MOKNGFlg = 0
1597     Ovrd 100
1598     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1599     ' �b��
1600     Ovrd 5
1601     Mvs PScrewPosition(10),-10    ' �p���b�g��˂�����S�@�̏��ֈړ�
1602 '    Ovrd MOvrdA
1603     '�b��}�X�N
1604 '    M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
1605 '    Dly 0.1
1606 '    M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1607 '    'Spd 8.3 '�O�����C�h100  �������C�h60   '���C�h100-40�@100%�FSpd�@15�@'�˂����ߑ��x�ݒ�
1608 '    Spd MSpdA               '�l�W���ߎ�Spd�ʐݒ�
1609     ' �b��ړ��̂�
1610     Mvs PScrewPosition(10)
1611 '    '
1612 '    Dly 0.1
1613 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1614 '    Wait M_In(11584)=1          '����/�G���[���o
1615 '    Dly 0.1
1616 '    Spd M_NSpd
1617 '    '
1618 '    If M_In(X28_Driver)=1 Then  '�˂��g�[�^���G���[���o��
1619 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1620 '        Dly 0.1
1621 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1622 '        Dly 0.1
1623 '        M_Out(Y63_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1624 '        Dly 0.1
1625 '        M_Out(Y65_Driver)=0     '�v���O���������@F1
1626 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1627 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
1628 '        MOKNGFlg = -1
1629 '        ScrewTight_S2 = 0
1630 '    Else
1631 '        Wait M_In(X29_Driver)=1 ' ���튮����
1632 '        Dly 0.1
1633 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1634 '        Dly 0.1
1635 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O����
1636 '        Dly 0.1
1637 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
1638 '        Dly 0.1
1639 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1640 '        ScrewTight_S2 = 1
1641 '    EndIf
1642 ' �b��
1643     Ovrd 10
1644     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1645     Ovrd 100
1646 FEnd
1647 '
1648 '��ScrewGet_S3
1649 ''' <summary>
1650 ''' �˂������@����˂��𓾂�
1651 ''' </summary>
1652 '''<param name="%"></param>
1653 '''         PScrewPos(1)    �F�˂�������̂˂����
1654 '''         PScrewPos(2)    �F�˂���������_
1655 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1656 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1657 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1658 '''<returns>����
1659 '''         0=�ُ�I���A1=����I���A-1=M�l�W�Z���T�[NG�A-2=M�l�W�Z���T�[ON�A-3=�z���G���[
1660 '''</returns>
1661 ''' <remarks>
1662 ''' Date   : 2021/07/07 : M.Hayakawa
1663 ''' </remarks>'
1664 Function M% ScrewGet_S3(ByVal PScrewPosition())
1665     ScrewGet_S3 = 0
1666     MMScrewJudge% = 0
1667     '�˂������평������G���[�`�F�b�N
1668 ' ���b��폜
1669 '    Wait M_In(X34_ScrewReady1)=1 '�˂�������S��Ready�ɂȂ�܂ő҂@���@���b���҂���Ready�ɂȂ�Ȃ���Δ�����v���O�������K�v�H
1670 '    Ovrd 100
1671 '    If M_In(X33_SS2)=0 Then  'M�˂����o�Z���T��OFF�i�̏�j���Ă����ꍇ
1672 '        Ovrd 30
1673 '        Mvs,-80             '���̏ꏊ����80mm���ֈړ�
1674 '        Mov PInitPos19049   '19049�����ʒu�ֈړ�
1675 '        M_Out(Y6A_VV1)=0    '�˂��z�� Off
1676 '        'NG�Ƃ��Ă����̊֐����甲����
1677 '        ScrewGet_S3 = -1
1678 '        MMScrewJudge% = 1
1679 '        MCommentD1001 = 61
1680 '    EndIf
1681 '    If ScrewGet_S3 = 0 Then
1682 '        'S�^�C�g�p�˂������@��M�˂����������Ă��Ȃ����Ď�
1683 '        MMScrewJudge% = 0 'MMScrewJudge������������
1684 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
1685 '        If MRtn = 0 Then
1686 '            Ovrd 30
1687 '            Mvs,-80            '���̏ꏊ����50mm���ֈړ�
1688 '            Mov PInitPos19049  '19049�����ʒu�ֈړ�
1689 '            MMScrewJudge% = 2
1690 '            MRtn = All_CLamp_Release()'�S�ẴN�����v�����֕���
1691 '            MCnt% = 2   '2��ݒ�
1692 '            MCommentD1001 = 62
1693 '        EndIf
1694 '        If MMScrewJudge% = 2 Then
1695 '            ScrewGet_S3 = -2
1696 '        EndIf
1697 '    EndIf
1698 '    'M�l�W���肪ON�̏ꍇ NG�Ƃ��Ċ֐��𔲂���
1699 '    If MMScrewJudge% = 2 Then
1700 '        ScrewGet_S3 = -2
1701 '    EndIf
1702     'S�l�W�p�˂����Y��M�l�W�����m�F�p�����܂�
1703     Ovrd 100
1704     Spd M_NSpd
1705     If MMScrewJudge% = 0 Then
1706         ScrewGet_S3 = 0
1707         M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
1708         MScrewCnt% = 0
1709         MFinCnt% = 2
1710 '        For MCnt% = 0 To MFinCnt%
1711             Mov PScrewPosition(2)        ' �˂������@���_
1712             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
1713             Ovrd 80
1714             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1715             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
1716             Mvs PScrewPosition(10), 1.2
1717             M_Out(Y6A_VV1)=1        ' �˂��z���@ON
1718             '�r�b�g��]
1719             M_Out(Y60_Driver)=1
1720             Dly 0.2
1721             '
1722             Ovrd 100
1723             JOvrd M_NJovrd
1724             Spd M_NSpd
1725             '�l�W�z���m�F�ʒu�ړ�
1726             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
1727             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
1728             '�r�b�g��]��~
1729             'M_Out(Y60_Driver)=0
1730             '
1731             '1�b�ԃl�W�z���m�F
1732 ' �ȉ��b��폜
1733 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1734 '            'MRtn = 0'�����G���[
1735 '            '�z���G���[�̏ꍇ
1736 '            '�l�W���˂����Y�ɖ߂�
1737 '            If MRtn = 0 Then
1738 '                Ovrd 30
1739 '                '�r�b�g��]��~
1740 '                M_Out(Y60_Driver)=0
1741 '                '�l�W�����@���
1742 '                Mvs PScrewPos(1)
1743 '                '�X�ɏ��
1744 '                Mov PScrewPos(1), -75
1745 '                '�l�W�̂Ĉʒu
1746 '                Mov PScrewFeedS021
1747 '                '�z��OFF
1748 '                M_Out(Y6A_VV1)=0 '�˂��z���@OFF
1749 '                Dly 0.2
1750 '                '�j��ON
1751 '                M_Out(Y6B_VB1)=1 '�^��j��ON
1752 '                '�r�b�g��]
1753 '                M_Out(Y61_Driver)=1
1754 '                Dly 0.5
1755 '                '
1756 '                Ovrd 100
1757 '                JOvrd M_NJovrd
1758 '                Spd M_NSpd
1759 '                '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1760 '                Mov PScrewFeedS021, 10
1761 '                Mov PScrewFeedS021
1762 '                Dly 0.1
1763 '                Mov PScrewFeedS021, 10
1764 '                Mov PScrewFeedS021
1765 '                '
1766 '                '�l�W�����҂�
1767 '                '�r�b�g��]��~
1768 '                M_Out(Y61_Driver)=0
1769 '                Dly 0.1
1770 '                '�j��OFF
1771 '                M_Out(Y6B_VB1)=0 '�^��j��OFF
1772 '                '
1773 '                '
1774 '                '�˂��������Ƃ��āA�ړ��X�ɏ��
1775 '                Mov PScrewPos(1), -75
1776 '                Ovrd 100
1777 '                Spd M_NSpd
1778 '                '�l�W�����@���
1779 '                Mvs PScrewPos(1)
1780 '                '
1781 '                ScrewGet_S3 = -3
1782 '                Break
1783 '                '
1784 '            Else
1785 '                MCnt% = MFinCnt%
1786 '                ScrewGet_S3 = 0
1787 '            EndIf
1788 '        Next  MCnt%
1789         '
1790         Ovrd 100
1791         Spd M_NSpd
1792         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
1793         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
1794         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
1795         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
1796         '������x�z���m�F
1797 ' �ȉ��b��폜
1798 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1799 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
1800 '            MCommentD1001 = 94
1801 '            MCommentD1002 = 95
1802 '            ScrewGet_S3 = -3
1803 '        EndIf
1804 '        If MRtn = 1 Then      '�z��OK�̏ꍇ
1805 '            ScrewGet_S3 = 1
1806 '        EndIf
1807 '        Break
1808     Else
1809         'M�l�W
1810         If MMScrewJudge% = 2 Then
1811             ScrewGet_S3 = -2
1812         EndIf
1813     EndIf
1814 FEnd
1815 '
1816 '��fnKEY_WAIT()
1817 ''' <summary>
1818 ''' GOT����̃L�[���͑҂�
1819 ''' </summary>
1820 '''<returns>1�F��~    2�F����
1821 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
1822 '''         5�FNG
1823 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
1824 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
1825 '''</returns>
1826 ''' <remarks>
1827 ''' Date   : 2021/07/07 : M.Hayakawa
1828 ''' </remarks>'
1829 Function M% fnKEY_WAIT()
1830     fnKEY_WAIT = 0
1831     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
1832     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
1833     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
1834     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1835     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
1836     Dly 0.2
1837     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
1838     MLocalLoopFlg=1
1839     While MLocalLoopFlg=1
1840         If M_In(11345) = 1 Then         '��~   M5345
1841             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
1842             fnKEY_WAIT = 1
1843             MLocalLoopFlg=-1
1844             Break
1845         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
1846             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
1847             fnKEY_WAIT = 2
1848             MLocalLoopFlg=-1
1849             Break
1850         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
1851             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
1852             fnKEY_WAIT = 3
1853             MLocalLoopFlg=-1
1854             Break
1855         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
1856             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
1857             fnKEY_WAIT = 4
1858             MLocalLoopFlg=-1
1859             Break
1860         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
1861             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
1862             fnKEY_WAIT = 5
1863             MLocalLoopFlg=-1
1864             Break
1865             '
1866         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
1867             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
1868             fnKEY_WAIT = MRobotInit1%
1869             MLocalLoopFlg=-1
1870             Break
1871             '
1872         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
1873             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
1874             fnKEY_WAIT = MRobotInit2%
1875             MLocalLoopFlg=-1
1876             Break
1877             '
1878         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
1879             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
1880             fnKEY_WAIT = MRobotInit3%
1881             MLocalLoopFlg=-1
1882             Break
1883             '
1884         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
1885             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
1886             fnKEY_WAIT = MRobotInit4%
1887             MLocalLoopFlg=-1
1888             Break
1889             '
1890         Else
1891         EndIf
1892     WEnd
1893     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
1894     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
1895 FEnd
1896 '
1897 '�� fnAUTO_CTL
1898 ''' <summary>
1899 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
1900 ''' </summary>
1901 ''' <remarks>
1902 ''' Date   : 2021/07/07 : M.Hayakawa
1903 ''' </remarks>
1904 Function M% fnAUTO_CTL
1905     fnAUTO_CTL = 0
1906     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1907     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
1908     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1909     '
1910     If M_Svo=0 Then             '�T�[�{ON�m�F
1911         Servo On
1912     EndIf
1913     Wait M_Svo=1
1914 FEnd
1915 '
1916 '�� fnWindScreenOpen
1917 ''' <summary>
1918 ''' �E�B���h��ʂ̕\���A��\���ݒ�
1919 ''' </summary>
1920 '''<param name="%"></param>
1921 '''<param name="%"></param>
1922 '''<param name="%"></param>
1923 '''<param name="%"></param>
1924 ''' <remarks>
1925 ''' �R�����gD1001, D1002, D1003�̐ݒ�
1926 ''' MWindReSet = 0     ��ʔ�\��
1927 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
1928 ''' MWindErrScr = 10    �G���[��� D1001, D1002
1929 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
1930 ''' Date   : 2021/07/07 : M.Hayakawa
1931 ''' </remarks>
1932 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
1933     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
1934         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
1935     EndIf
1936     '
1937     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
1938         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
1939     EndIf
1940     '
1941     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
1942        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
1943     EndIf
1944     '
1945     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
1946     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
1947     Dly 0.5
1948     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
1949 FEnd
1950 '
1951 '��FnCtlValue2
1952 ''' <summary>
1953 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
1954 ''' </summary>
1955 ''' <param name="MCtlNo%"></param>
1956 ''' <remarks>
1957 ''' Date : 2022/04/28 �n��
1958 ''' </remarks>
1959 '''
1960 '''  1�F������       �{�P
1961 '''  2�F�g���n�j��   �{�P
1962 '''  3�F�g���m�f��   �{�P (���g�p)
1963 '''  4�F�z���G���[�� �{�P
1964 ''' 99�F�Ǐ��J�n�M�� OFF
1965 '''
1966 Function M% FnCtlValue2(ByVal MCtlNo%)
1967     FnCtlValue2 = 1
1968     Select MCtlNo%
1969         Case 1        '�������{�P
1970             M_Out(12569) = 0             '�����݊J�n�M��OFF
1971             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
1972             MInputQty = M_In16(11600)    '��������M
1973             MInputQty = MInputQty + 1    '�������{�P
1974             M_Out16(12592) = MInputQty   '���������M
1975             M_Out(12569) = 1             '�����݊J�n�M��ON
1976             Break
1977             '
1978         Case 2        '�g���n�j���{�P
1979             M_Out(12569) = 0             '�����݊J�n�M��OFF
1980             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
1981             MAssyOkQty = M_In16(11616)   '�g��OK����M
1982             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
1983             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
1984             M_Out(12569) = 1             '�����݊J�n�M��ON
1985             Break
1986             '
1987         Case 4        '�z���G���[���{�P
1988             M_Out(12569) = 0                       '�����݊J�n�M��OFF
1989             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
1990             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
1991             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
1992             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
1993             M_Out(12569) = 1                       '�����݊J�n�M��ON
1994             Break
1995             '
1996         Case 99        '�Ǐ��J�n�M��OFF
1997             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
1998             M_Out(12569) = 0        '�����݊J�n�M��OFF
1999             Break
2000             '
2001     End Select
2002     Exit Function
2003 FEnd
2004 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2005 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2006 '-------------------------------------------------------------------------------
2007 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2008 '   ����
2009 '       PInspPos()      �F�����ʒu
2010 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2011 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2012 '       MInspCnt%       �F�����ʒu��
2013 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2014 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2015 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2016 '   �߂�l�F����
2017 '       0=�ُ�I���A1=����I��
2018 '
2019 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2020 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2021 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2022 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2023 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2024 '-------------------------------------------------------------------------------
2025     '----- �����ݒ� -----
2026     Cnt 0                                                           '�ړ�����������(�����l=0)
2027     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2028 '    Cnt 1,0.1,0.1
2029     '�ϐ��錾�E������
2030     Def Inte MNum                                                   '�����ԍ�(������1�`)
2031     MNum% = 1                                                       '�����ԍ������l�ݒ�
2032     Def Inte MEndFlg                                                '�����I���t���O
2033     MEndFlg% = 0
2034     '
2035     '����G�ԍ��ݒ�v���E�������s�v��off
2036     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2037     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2038     '�G���[�ԍ��N���A
2039     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2040     M_Out16(MOUT_InspErrNum) = MInspErrNum
2041     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2042     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2043     '
2044     'Insight Ready check?
2045     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2046         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2047         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2048         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2049         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2050         Exit Function
2051     EndIf
2052     '
2053     '�����ʒu���m�F
2054     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2055         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2056         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2057         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2058         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2059         Exit Function
2060     EndIf
2061     '
2062     '
2063     '
2064     '----- ���C������ -----
2065     '�ݒ肳�ꂽ�����ʒu�����̌������s
2066     While( MEndFlg% = 0 )
2067         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2068         MSetGrNumRetryExitFlg = 0
2069         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2070         While( MSetGrNumRetryExitFlg = 0 )
2071         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2072             '
2073             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2074             '
2075             '----- �����O���[�v�ԍ��ݒ� -----
2076             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2077             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2078             '
2079             '�����ʒu�ֈړ��E�ړ������҂�
2080             Mvs PInspPos( MNum% )                                       '�ړ�
2081             Dly 0.05                                                    '�ړ�������Delay
2082             '
2083             '�����O���[�v�ԍ��ݒ�I���m�F
2084             M_Timer(1) = 0
2085             MExitFlg = 0
2086             While( MExitFlg = 0 )
2087                 '����G�ݒ萳��I��?
2088                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2089                     MExitFlg = 1
2090                 '
2091                 '����G�ݒ�ُ�I��?
2092                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2093                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2094                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2095                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2096                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2097                     EndIf
2098                     MExitFlg = 1
2099                 '
2100                 'timeout�`�F�b�N
2101                 ElseIf 1000 < M_Timer(1) Then
2102                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2103                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2104                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2105                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2106                     EndIf
2107                     MExitFlg = 1
2108                 EndIf
2109             WEnd
2110             '
2111             '����G�ԍ��ݒ�v��off
2112             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2113             '
2114             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2115             'NG�Ȃ���Δ�����
2116             If MCurrentStepErr = 0 Then
2117                 MSetGrNumRetryExitFlg = 1
2118             Else
2119                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2120                 If MSetGrNumRetryCnt = 0 Then
2121                     MSetGrNumRetryExitFlg = 1
2122                 Else
2123                     'Retry�ց@���̑O��Delay
2124                     Dly 0.5
2125                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2126                 EndIf
2127             EndIf
2128             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2129             '
2130         WEnd
2131         '
2132         '
2133         '
2134         '----- �������s -----
2135         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2136             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2137                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2138                 MInspRetryExitFlg = 0
2139                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2140                 While( MInspRetryExitFlg = 0 )
2141                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2142                     '
2143                     '���������m�F
2144                     MRetryCnt = MRetryCnt - 1
2145                     M_Timer(1) = 0
2146                     MExitFlg = 0
2147                     While( MExitFlg = 0 )
2148                     '���������҂�
2149                         '����OK�I��?
2150                         If M_In( MIN_IS_InspOK% ) = 1  Then
2151                             MJudgeOKFlg = 1                         '����OK�t���OON
2152                             MExitFlg = 1
2153                         '
2154                         '����NG�I��?
2155                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2156                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2157                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2158                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2159                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2160                                 EndIf
2161                             EndIf
2162                             MExitFlg = 1
2163                         '
2164                         '�����ُ�I��(IS timeout)?
2165                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2166                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2167                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2168                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2169                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2170                                 EndIf
2171                             EndIf
2172                             MExitFlg = 1
2173                         '
2174                         'timeout�`�F�b�N
2175                         ElseIf 3000 < M_Timer(1) Then
2176                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2177                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2178                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2179                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2180                                 EndIf
2181                             EndIf
2182                             MExitFlg = 1
2183                         EndIf
2184                     WEnd
2185                     '
2186                     '�����J�n�v��off
2187                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2188                     '
2189                     'OK�Ȃ甲����
2190                     If MJudgeOKFlg = 1 Then
2191                         MInspRetryExitFlg = 1
2192                     Else
2193                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2194                         If MRetryCnt = 0 Then
2195                             MInspRetryExitFlg = 1
2196                         Else
2197                             'Retry�ց@���̑O��Delay
2198                             Dly 0.3
2199                         EndIf
2200                     EndIf
2201                     '
2202                 WEnd
2203             EndIf
2204         EndIf
2205         '
2206         '
2207         '
2208         MNum% = MNum% + 1                                           '����Step+1
2209         '�����I���m�F�@�����I���t���O�Z�b�g
2210         If (MInspCnt% < MNum% ) Then
2211             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2212         EndIf
2213         'NG���������s������
2214         If MInspErrNum <> 0 Then                                    'NG����?
2215             If MNgContinue% <> 1 Then                               'NG���s?
2216                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2217             EndIf
2218         EndIf
2219     WEnd
2220     '
2221     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2222     If 0 < MZAxis% Then
2223         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2224         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2225         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2226     EndIf
2227     Fine 0 , P
2228     '
2229     '�߂�l�ݒ�
2230     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
2231         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2232     Else
2233         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2234         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2235         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2236     EndIf
2237     '
2238 FEnd
2239 '
2240 ' ��ISInspection
2241 ''' <summary>
2242 ''' Insight�ɂ��摜�����������s
2243 ''' </summary>
2244 '''<param name="PInspPos()">�����ʒu</param>
2245 '''<param name="MInspGrNum%()">�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j</param>
2246 '''             PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2247 '''<param name="MInspCnt%">�����ʒu��</param>
2248 '''<param name="MZAxis%">�I������Z���ޔ����W�i-1:�����j</param>
2249 '''             �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2250 '''<param name="MNgContinue%">=1�Ō����G���[�ENG�������ɑSStep�̌������s��</param>
2251 '''<returns>    ���� 0=�ُ�I���A1=����I��</returns>
2252 '''         MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2253 '''         MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���"
2254 ''' <remarks>
2255 ''' Date   : 2021/07/07 : M.Hayakawa
2256 ''' </remarks>
2257 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2258 '    '�摜�g�p�m�F 0<- �摜�m�F�����̏ꍇ
2259 '    If M_In(11369) = 0 Then            'toRBT_�g�p�m�F
2260 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2261 '    EndIf
2262 ''
2263 '    Cnt 0                                                       '�ړ�����������(�����l=0)
2264 '    Fine 0.05,P                                                 '�ʒu���ߊ��������ݒu�@0.05mm
2265 '    MNum% = 1                                                   '�����ԍ������l�ݒ�
2266 '    Def Inte MEndFlg                                            '�����I���t���O
2267 '    MEndFlg% = 0
2268 '    '
2269 '    '�G���[�ԍ��N���A
2270 '    MInspErrNumSub = 0                                          '�������s�G���[�ԍ�sub
2271 '    MInspErrNum = 0                                             '�������s�G���[�ԍ�
2272 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2273 '    MInspNGStepNum = 0                                          '�������sNGStep�ԍ�
2274 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2275 '    '
2276 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready off�Ȃ�I��
2277 '        MInspErrNum = 20                                        '�������s�G���[�ԍ� 20 Insight offline
2278 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2279 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2280 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2281 ''
2282 '    EndIf
2283 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2284 '    '
2285 '    '�����ʒu���m�F
2286 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2287 '        MInspErrNum = 21                                        '�����f�[�^�Ȃ� 21�@����<1
2288 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2289 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2290 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2291 ''
2292 '    EndIf
2293 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2294 '    '
2295 '    '�ݒ肳�ꂽ�����ʒu�����̌������s
2296 '    While( MEndFlg% = 0 )
2297 '        '�����I���m�F�@�����I���t���O�Z�b�g
2298 '        If (MInspCnt% < MNum% ) Then
2299 '            MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2300 '        EndIf
2301 '        '
2302 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����J�n�@INSPTAST1
2303 '        If MEndFlg% = 0 Then
2304 '            M_01# = MInspGrNum%(MNum%)                          '����G�ԍ����n��
2305 '        EndIf
2306 '        M_02# = MEndFlg%                                        '�����I���t���O���n��
2307 '        M_05# = MNum%                                           '�����ԍ�(������1�`)
2308 '        '�^�X�N�@����G�ݒ�t���O���n��
2309 '        If MEndFlg% = 0 Then
2310 '            If 0 < MInspGrNum%(MNum%) Then
2311 '                M_03# = 1
2312 '            Else
2313 '                M_03# = 0
2314 '            EndIf
2315 '        Else
2316 '            M_03# = 0
2317 '        EndIf
2318 '        '�^�X�N�@�������ʊm�F�t���O���n��
2319 '        If 1 < MNum% Then
2320 '            If 0 < MInspGrNum%(MNum%-1) Then
2321 '                M_04# = 1
2322 '            Else
2323 '                M_04# = 0
2324 '            EndIf
2325 '        Else
2326 '            M_04# = 0
2327 '        EndIf
2328 '        '
2329 '        '�^�X�N�����J�n
2330 '        M_00# = 1                                               'TASK�����J�n
2331 '        '�^�X�N�����J�n�m�F
2332 '        M_Timer(1) = 0
2333 '        MExitFlg = 0
2334 '        While( MExitFlg = 0 )
2335 '            '�����J�n�����m�F
2336 '            If M_00# = 0 And M_10# = 8 Then
2337 '                MExitFlg = 1
2338 '            EndIf
2339 '            'timeout�`�F�b�N
2340 '            If 2000 < M_Timer(1) Then
2341 '                If MNgContinue% = 1 Then                        'NG���s?
2342 '                    MInspErrNumSub = 36                         '�G���[�ԍ��ݒ�36
2343 '                Else
2344 '                    MInspErrNum = 36                            '�G���[�ԍ��ݒ�36
2345 '                EndIf
2346 '                MExitFlg = 1
2347 '            EndIf
2348 '        WEnd
2349 '        '
2350 '        '�����ʒu�ֈړ��E�ړ������҂�
2351 '        If 0 = MInspErrNum Then
2352 '            If MEndFlg% = 0 Then
2353 '                Mvs PInspPos( MNum% )                           '�ړ�
2354 '            EndIf
2355 '        EndIf
2356 '        '
2357 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����I���҂��@INSPTAST1
2358 '        If 0 = MInspErrNum Then
2359 '            M_Timer(1) = 0
2360 '            MExitFlg = 0
2361 '            While( MExitFlg = 0 )
2362 '                '���������҂��i����I���j
2363 '                If M_10# = 1 Then
2364 '                    MExitFlg = 1
2365 '                EndIf
2366 '                '���������҂��i�ُ�I���j
2367 '                If M_10# = 0 Then
2368 '                    If MNgContinue% = 1 Then                    'NG���s?
2369 '                        MInspErrNumSub = M_12#                  '�G���[�ԍ��ݒ�@M12
2370 '                    Else
2371 '                        MInspErrNum = M_12#                     '�G���[�ԍ��ݒ�@M12
2372 '                    EndIf
2373 '                    MExitFlg = 1
2374 '                EndIf
2375 '                'timeout�`�F�b�N
2376 '                If 5000 < M_Timer(1) Then
2377 '                    If MNgContinue% = 1 Then                    'NG���s?
2378 '                        MInspErrNumSub = 31                     '�G���[�ԍ��ݒ�31
2379 '                    Else
2380 '                        MInspErrNum = 31                        '�G���[�ԍ��ݒ�31
2381 '                    EndIf
2382 '                    MExitFlg = 1
2383 '                EndIf
2384 '            WEnd
2385 '        EndIf
2386 '        '
2387 '        '�������ʊm�F
2388 '        If 0 = MInspErrNum Then
2389 '            If 1 < MNum% Then
2390 '                If 0 < MInspGrNum%(MNum%-1) Then                '��������?
2391 '                    If M_11# = 2 Then                           '����NG?
2392 '                        If MNgContinue% = 1 Then                'NG���s?
2393 '                            If MInspNGStepNum = 0 Then          'NG������?
2394 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2395 '                            EndIf
2396 '                            MInspErrNumSub = 32                 '�G���[�ԍ��ݒ� 32:����NG
2397 '                        Else
2398 ''                            MInspNGStepNum = MNum% - 1          '�������sNGStep�ԍ��ݒ�
2399 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2400 '                            MInspErrNum = 32                    '�G���[�ԍ��ݒ� 32:����NG
2401 '                        EndIf
2402 '                   EndIf
2403 '                EndIf
2404 '            EndIf
2405 '        EndIf
2406 '        '
2407 '        '�G���[�Ȃ猟�����f�I������̂�Loop���甲���邽�ߏI���t���O�Z�b�g
2408 '        If 0 <> MInspErrNum Then
2409 '            MEndFlg% = 1
2410 '        EndIf
2411 '        '
2412 '        '�������s�A�捞�����҂�
2413 '        If 0 = MInspErrNum Then
2414 '            If MEndFlg% = 0 Then
2415 '                If 0 < MInspGrNum%(MNum%) Then                  '��������?
2416 '                    M_Out(MOUT_IS_Insp%) = 1                    '�������s�v��on
2417 '                    '�捞�����m�F
2418 '                    M_Timer(1) = 0
2419 '                    MExitFlg = 0
2420 '                    While( MExitFlg = 0 )
2421 '                        '���������҂�
2422 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2423 '                            MExitFlg = 1
2424 '                        EndIf
2425 '                        'timeout�`�F�b�N
2426 '                        If 2000 < M_Timer(1) Then
2427 '                            If MNgContinue% = 1 Then            'NG���s?
2428 '                                MInspErrNumSub = 33             '�G���[�ԍ��ݒ�33
2429 '                            Else
2430 '                                MInspErrNum = 33                '�G���[�ԍ��ݒ�33
2431 '                            EndIf
2432 '                            MExitFlg = 1
2433 '                        EndIf
2434 '                    WEnd
2435 '                EndIf
2436 '                '
2437 '            EndIf
2438 '        EndIf
2439 '        MNum% = MNum% + 1
2440 '    WEnd
2441 '    '
2442 '    '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2443 '    If 0 < MZAxis% Then
2444 '        PCurrentPos = P_Curr                                    '���݈ʒu�擾
2445 '        PCurrentPos.Z = MZAxis%                                 'Z����ݒ�
2446 '        Mvs PCurrentPos                                         '���݈ʒu���ֈړ�
2447 '    EndIf
2448 '    '
2449 '    'NG���s������
2450 '    If MNgContinue% = 1 Then                                    'NG���s?
2451 '        MInspErrNum = MInspErrNumSub                            '�G���[�ԍ��ݒ�
2452 '    EndIf
2453 '    '
2454 '    '�߂�l�ݒ�
2455 '    If MInspErrNum = 0 Then
2456 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2457 '    Else
2458 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2459 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2460 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2461 '    EndIf
2462 '    '
2463 '*ISInspection_End
2464 'FEnd
2465 '
2466 '��InitialZoneB
2467 ''' <summary>
2468 ''' ����~��̕��A����
2469 ''' 1)���ޔ��@Z������Ɉړ�
2470 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
2471 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
2472 ''' 4)�C�j�V�����|�W�V�����ֈړ�
2473 ''' </summary>
2474 ''' <remarks>
2475 ''' Date : 2022/04/08 : N.Watanabe
2476 ''' </remarks>
2477 Function V fnInitialZoneB()
2478     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/04/26 �n��
2479 '
2480 '�p�����[�^
2481     Ovrd 5
2482 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2483 '    Cmp Pos, &B100011
2484 '
2485 '���A����J�n
2486 '
2487 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������
2488 *RecoveryChuckOpen
2489     PActive = P_Curr          '���݈ʒu���擾
2490     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
2491 'PProductOnRoboSet(�˂����{���i�u���ʒu)�́A�`���b�N���
2492     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2493         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2494             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2495                 MRecoveryChuckOpen = 1
2496             EndIf
2497         EndIf
2498     EndIf
2499 'PProductOnRoboGet(�˂����{���i���ʒu)�́A�`���b�N���
2500     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2501         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2502             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2503                 MRecoveryChuckOpen = 1
2504             EndIf
2505         EndIf
2506     EndIf
2507 '
2508     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2509     M_Out(12256) = 0                           '�{�̃`���b�N��OFF
2510     M_Out(12257) = 1                           '�{�̃`���b�N�JON
2511 '
2512     M_20# = 0                                  'KEY���͏�����
2513     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
2514     If MRtn = 1 Then M_Out(12257) = 0          '�{�̃`���b�N�JOFF
2515     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2516 '
2517     fErrorProcess(11,244,284,0)
2518     If M_20# = MNext% Then M_20# = MClear%
2519     If M_20# = MAbout% Then GoTo *RecoveryEnd
2520     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2521     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2522 '
2523     *RecoveryChuckOpenEnd
2524 '
2525 '�w�ʔ��
2526 'PPlateBackSet�`PPlateBackSet_6�̃G���A�ɂ���Ƃ��́A�{�̃`���b�N�J��
2527 '�EPPlateBackSet_6         '�o�H6
2528 '�EPPlateBackSet_5         '�o�H7
2529 '�EPPlateBackSet_4         '�o�H8
2530 '�EPPlateBackSet_3         '�o�H9
2531 '�EPPlateBackSet_2         '�o�H10
2532 '�EPPlateBackSet_1         '�o�H11
2533 '�EPPlateBackSet           '�w�ʔu���ʒu
2534 '��L�V�_�̂w���W�E�x���W�E�y���W��J6�������LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2535     PActive = P_Curr                    '���݈ʒu���擾
2536     JActive = J_Curr                    '���݈ʒu���擾
2537     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2538     If (PActive.X >= -35) And (PActive.X <= -5) Then
2539         If (PActive.Y >= 340) And (PActive.Y <= 510) Then
2540             If (PActive.Z >= 470) And (PActive.Z <= 560) Then
2541                 If (MJ6 >= 160) And (MJ6 <= 200) Then
2542                     M_Out(12256) = 0            '�{�̃`���b�N��OFF
2543                     M_Out(12257) = 1            '�{�̃`���b�N�JON
2544                 Dly 1.0
2545                 EndIf
2546             EndIf
2547         EndIf
2548     EndIf
2549 '
2550 '
2551 '�������@���ځA���ޔ����o���Ȃ����̑Ώ�
2552 '
2553     Ovrd 1
2554 'PProductOnRoboSet(Get)�`PProductOnRoboSet(Get)_2�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_2��
2555 '�EPProductOnRoboSet
2556 '�EPProductOnRoboSet_1
2557 '�EPProductOnRoboSet_2
2558 '�EPProductOnRoboGet
2559 '�EPProductOnRoboGet_1
2560 '�EPProductOnRoboGet_2
2561 '��L�U�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2562     PActive = P_Curr                    '���݈ʒu���擾
2563     JActive = J_Curr                    '���݈ʒu���擾
2564     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2565     If (PActive.X >= -35) And (PActive.X <= 0) Then
2566         If (PActive.Y >= 350) And (PActive.Y <= 420) Then
2567             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
2568                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2569                     Mvs PProductOnRoboSet_1
2570                     Dly 1.0
2571                     Mvs PProductOnRoboSet_2
2572                     Dly 1.0
2573                     Mov PProductOnRoboSet_3
2574                     Dly 1.0
2575                 EndIf
2576             EndIf
2577         EndIf
2578     EndIf
2579 '
2580 'PProductOnRoboSet(Get)_2�`PProductOnRoboSet(Get)_3�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_3��
2581 '�EPProductOnRoboSet_2
2582 '�EPProductOnRoboSet_3
2583 '�EPProductOnRoboGet_2
2584 '�EPProductOnRoboGet_3
2585 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2586     PActive = P_Curr                    '���݈ʒu���擾
2587     JActive = J_Curr                    '���݈ʒu���擾
2588     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2589     If (PActive.X >= -35) And (PActive.X <= 0) Then
2590         If (PActive.Y >= 280) And (PActive.Y <= 390) Then
2591             If (PActive.Z >= 410) And (PActive.Z <= 570) Then
2592                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2593                     Mvs PProductOnRoboSet_3
2594                     Dly 1.0
2595                 EndIf
2596             EndIf
2597         EndIf
2598     EndIf
2599 '
2600     Ovrd 5
2601 '
2602 '���ޔ�
2603     PActive = P_Curr
2604     Pmove = PActive
2605     Pmove.Z = 640           '���ޔ�����ꗥ�̍���
2606     If PActive.X > 550 Then
2607         Pmove.Z =550        '�p���b�g��ɘr��L�΂��Ă���Ƃ���640�܂ŏグ���Ȃ��ׁA��O���u
2608     EndIf
2609     If PActive.Z < Pmove.Z Then
2610         Mvs Pmove
2611     EndIf
2612     Dly 1.0
2613 'J1���ȊO��ޔ��|�W�V�����ֈړ�
2614     JActive = J_Curr
2615     Jmove = JTaihi
2616     Jmove.J1 = JActive.J1        'J1���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
2617     Jmove.J6 = JActive.J6        'J6���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
2618     Mov Jmove
2619     Dly 1.0
2620 'J1���݂̂�ޔ��|�W�V�����ֈړ�
2621     Mov JTaihi
2622     Dly 1.0
2623 '�C�j�V�����|�W�V�����ֈړ�
2624     Mov PInitialPosition
2625     Cmp Off
2626     Ovrd 100
2627 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n
2628     If M_In(11856) = 0 Then                 ' ��~���̂�
2629         fnAutoScreenComment(501)            ' ��ԕ\��[�l�W���ߋ@�����^�]�J�n��] 2022/04/25 �n��
2630         M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
2631         MRet = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
2632         If MRet = 0 Then
2633         Else
2634             M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
2635         EndIf
2636     EndIf
2637     M_Out(12262) = 0            '�ʒu���ߏoOFF
2638     M_Out(12263) = 1            '�ʒu���ߖ�ON
2639     fErrorProcess(11,253,281,0)
2640 *RecoveryEnd
2641     Exit Function
2642 FEnd
2643 '
2644 '
2645 '��fnAutoScreenComment
2646 ''' <summary>
2647 ''' ���C����ʂ̓���󋵕\��
2648 ''' �R�����gD1005�̐ݒ�
2649 ''' </summary>
2650 '''<param name="McommentD1005%">�R�����gID</param>
2651 ''' <remarks>
2652 ''' Date   : 2021/07/07 : M.Hayakawa
2653 ''' </remarks>
2654 Function fnAutoScreenComment(ByVal McommentD1005%)
2655     M_Out16(12576) = McommentD1005%
2656 FEnd
2657 '
2658 '��fnRoboPosChk
2659 ''' <summary>
2660 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2661 ''' </summary>
2662 '''<param name="MINNumber%">���͔ԍ�</param>
2663 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2664 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2665 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2666 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2667 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2668 ''' <remarks>
2669 ''' Date   : 2021/07/07 : M.Hayakawa
2670 ''' </remarks>
2671 Function M% fnRoboPosChk
2672     fnRoboPosChk = 0
2673     MRet = fnStepRead()
2674     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2675     '�E�B���h��ʐ؊���
2676     If MRBTOpeGroupNo > 5 Then
2677         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2678         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2679         Dly 0.2
2680         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2681         Dly 1.5
2682         '
2683         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2684         '
2685         MLoopFlg% = 1
2686         While MLoopFlg% = 1
2687             '
2688             '
2689             MKeyNumber% = fnKEY_WAIT()
2690             Select MKeyNumber%
2691                 Case Is = MAbout%       '��~
2692                     M_20# = MAbout%
2693                     MLoopFlg% = -1
2694                     Break
2695                 Case Is = MNext%        '����
2696                     'MLoopFlg% = -1
2697                     Break
2698                 Case Is = MContinue%    '�p��
2699                     M_20# = MContinue%
2700                     MLoopFlg% = -1
2701                     Break
2702                 Default
2703                     Break
2704             End Select
2705         WEnd
2706     EndIf
2707     '
2708     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
2709         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2710         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
2711         Select MRBTOpeGroupNo
2712             Case Is = 5                          '�������Ȃ�
2713                 Break
2714             Case Is = 10                         '�����ʒu�֖߂�
2715                 'Mov PTEST001
2716                 Break
2717             Case Is = 15                         '�����ʒu�֖߂�
2718                 'Mov PTEST002
2719                 Dly 0.5
2720                 'Mov PTEST001
2721                 Dly 0.5
2722                 Break
2723             Default
2724                 Break
2725         End Select
2726         '
2727         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
2728         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
2729         MRBTOpeGroupNo = 5
2730         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
2731         Dly 1.0
2732         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
2733         fnRoboPosChk = 1                        '�����ʒu������s
2734         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2735     EndIf
2736     Exit Function
2737 FEnd
2738 '
2739 '��frInCheck
2740 ''' <summary>
2741 ''' �Z���T�[IN�`�F�b�N
2742 ''' </summary>
2743 '''<param name="MINNumber%">���͔ԍ�</param>
2744 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2745 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2746 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2747 ''' <remarks>
2748 ''' Date   : 2021/07/07 : M.Hayakawa
2749 ''' </remarks>
2750 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2751     M_Timer(4) = 0
2752     MloopFlg = 0
2753     While MloopFlg = 0
2754         MCrtTime& = M_Timer(4)
2755         If M_In(MINNumber%) = MCMPFLG% Then
2756             MloopFlg = 1
2757             frInCheck = 1
2758         ElseIf MCrtTime& > MTimeCnt& Then
2759             MloopFlg = 1
2760             frInCheck = 0
2761         EndIf
2762     WEnd
2763 FEnd
2764 '-----------------------------------------------
2765 '
2766 '�˂����ߋ@�ʐM�m�F
2767 '
2768 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2769 'fScrewTcomChk = 0�@�F����I��
2770 '          �@ �@ -1 �F�ُ�I��
2771 '-----------------------------------------------
2772 Function M% fScrewTcomChk
2773 *ReCheckScewTcomChk
2774     fScrewTcomChk = 0
2775     '�ʐM�m�F���M
2776     M_Out(MOUT_ScwT_ComChk%) = MOn%
2777     '�ʐM�m�F��M�ҋ@
2778 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
2779     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
2780     '�ʐM�m�F���M�I��
2781     M_Out(MOUT_ScwT_ComChk%) = MOff%
2782     If MRtn = 0 Then
2783         fScrewTcomChk = -1
2784     EndIf
2785     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
2786  '
2787 FEnd
2788 '
2789 '
2790 '-----------------------------------------------
2791 '
2792 '�˂����ߊJ�n���M
2793 '
2794 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2795 'fScrewTStart = 0�@�F����I��
2796 '           �@�@-1 �F�ُ�I��
2797 '-----------------------------------------------
2798 Function M% fScrewTStart
2799     fScrewTStart = 0
2800     nRet% = 0
2801     '�˂����ߊJ�n�ҋ@����M
2802 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
2803     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
2804     If MRtn = 0 Then nRet% = -1
2805     If MRtn = 0 Then GoTo *ScrewStartERROR      '�J�n�ł��Ȃ������ꍇ�W�����v
2806     Dly 0.1
2807     '�˂����ߊJ�n��M�𑗐M
2808     M_Out(MOUT_ScwT_ST%) = MOn%
2809     Dly 0.5
2810     'Wait M_In(MTEST_KEY%) = MOn%
2811     '�˂����ߊJ�n���M�I��
2812     M_Out(MOUT_ScwT_ST%) = MOff%
2813     '
2814 *ScrewStartERROR
2815     fScrewTStart = nRet%
2816 FEnd
2817 '
2818 '
2819 '
2820 '-----------------------------------------------
2821 '
2822 '�˂����ߊ�����M
2823 '
2824 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2825 'fScewTFinish = 0�@�F����I��
2826 '          �@ �@-1 �F�ُ�I��
2827 '-----------------------------------------------
2828 Function M% fScewTFinish
2829 *ReCheckScewTFinish
2830     fScewTFinish = 0
2831     '�˂����ߊ����ҋ@����M
2832 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
2833     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
2834     If MRtn = 0 Then
2835         fScewTFinish = -1
2836     EndIf
2837     If MRtn = 2 Then GoTo *ReCheckScewTFinish
2838     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
2839     Dly 0.1
2840     '�˂����ߊ�����M�𑗐M
2841     M_Out(MOUT_ScwT_FinOK%) = MOn%
2842     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
2843     '�˂����ߊJ�n���M�I��
2844     M_Out(MOUT_ScwT_FinOK%) = MOff%
2845     'Wait M_In(MTEST_KEY%) = MOn%
2846     '
2847 *ScewTFinish_ErrEnd
2848 FEnd
2849 '
2850 '
2851 '-----------------------------------------------
2852 '
2853 '����xx��~��M
2854 '
2855 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2856 'fScewTCaseStop = 0�@�F����I��
2857 '          �@   �@-1 �F�ُ�I��
2858 '-----------------------------------------------
2859 Function M% fScewTCaseStop(ByVal MCase%())
2860 *ReCheckScewTCaseStop
2861     fScewTCaseStop = 0
2862     '����xx��~����M
2863     Wait M_In(MCase%(1)) = MOn%
2864     MRtn = fTimeOutJudge(MCase%(1),MOn%)
2865     If MRtn = 0 Then
2866         fScewTCaseStop = -1
2867     EndIf
2868     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
2869     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
2870     Dly 0.1
2871     '����xx��~��M�𑗐M
2872     M_Out(MCase%(2)) = MOn%
2873     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
2874     '�˂����ߊJ�n���M�I��
2875     M_Out(MCase%(2)) = MOff%
2876 *ScewTCaseStop_ErrEnd
2877     '
2878 FEnd
2879 '
2880 '��fScrewTighenRoboCheck
2881 '<summary>
2882 '�˂����{�Ď�
2883 '</summary>
2884 '<param name = "MStopNum%"> ��~�ԍ�</param>
2885 '<returns>���� 0:�˂����{�ُ�I�� 1:OK </returns>
2886 '<make>
2887 '2021/12/2 �����V��
2888 '</make>
2889 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
2890     fnAutoScreenComment(503)    '��ԕ\��[�˂����{����I���҂�] 2022/04/26 �n��
2891     fScrewTighenRoboCheck = 1
2892     MScrewTighenRoboFlg% = 1    '�t���O�̏�����
2893     MCheck% = 0
2894     While MScrewTighenRoboFlg% = 1
2895         MCheck% = M_In16(11904)
2896         If M_In(MStopNum%) = 1 Then '��~�ʒu�܂ŗ�����
2897             MScrewTighenRoboFlg% = 0 '�֐��𔲂���
2898             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
2899         EndIf
2900         If MCheck% <> 0 Then
2901             fScrewTighenRoboError(MCheck%)
2902             Select M_20#
2903                 Case MAbout%            '��~�������ꂽ�ꍇ
2904                     M_Out(12869) = 1 Dly 1.0
2905                     MScrewTighenRoboFlg% = 0
2906                     fScrewTighenRoboCheck = 0   '�ُ�I��
2907                     Break
2908                 Case MNgProcess%        'NG�������ꂽ�ꍇ
2909                     M_Out(12873) = 1 Dly 1.0
2910                     MScrewTighenRoboFlg% = 0
2911                     fScrewTighenRoboCheck = 0   '�ُ�I��
2912                     Break
2913                 Case MContinue%             '���g���C�������ꂽ�ꍇ
2914                     M_20# = MClear%         'M_20#������
2915                     M_Out(12871) = 1 Dly 1.0
2916                     Break
2917                 Case MNext%                 '���ւ������ꂽ�ꍇ
2918                     M_20# = MClear%         'M_20#������
2919                     M_Out(12874) = 1 Dly 1.0
2920                     Break
2921             End Select
2922             Dly 0.5
2923         EndIf
2924     WEnd
2925 FEnd
2926 '
2927 '��fScrewTighenRoboError
2928 '<summary>
2929 '�˂����{�G���[����
2930 '</summary>
2931 '<param name = "ErrorCode%"> �G���[�ԍ�</param>
2932 '<make>
2933 '2021/12/2 �����V��
2934 '</make>
2935 Function fScrewTighenRoboError(ByVal MErrorCode%)
2936     MErrorScreenCode% = 0
2937     MErrorScreenCode% = MErrorCode% + 300
2938     fErrorProcess(11,MErrorScreenCode%,0,0)
2939 FEnd
2940 '
2941 '��fErrorProcess
2942 '<summary>
2943 '�G���[����
2944 '</summary>
2945 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
2946 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
2947 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
2948 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
2949 '<make>
2950 '2021/11/5 �����V��
2951 '</make>
2952 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
2953     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
2954     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
2955     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
2956     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
2957 *RETRY_ERR_PROCESS
2958      M_20# = MClear%     '������
2959 '        '�G���[�����L�q
2960         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
2961 '        'GOT KEY���͑҂�
2962         MKeyNumber = fnKEY_WAIT()
2963 '        '
2964         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2965             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2966             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2967             Break
2968          '
2969         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2970             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2971             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2972         '
2973         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2974             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2975             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2976          '
2977         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2978             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2979             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2980             Break
2981         '
2982         EndIf
2983         '
2984         If M_20# = MClear% Then *RETRY_ERR_PROCESS
2985 FEnd
2986 '
2987 '��fnTorqueCheck
2988 ''' <summary>
2989 ''' �g���N�`�F�b�N����p�̃��C��
2990 ''' </summary>
2991 ''' <remarks>
2992 ''' Date   : 2021/12/21 : H.AJI
2993 ''' </remarks>'
2994 Function M% fnTorqueCheck
2995     '�g���N�`�F�b�N�����M  �����n��~
2996     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
2997     '
2998     fnTorqueCheck = 0
2999     Ovrd 20
3000     Mov PInitialPosition              '�����ʒu�ړ�
3001     Ovrd 100
3002     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3003     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3004     Dly 0.2
3005     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3006     '
3007     'M6340  �g���N�`�F�b�N��M
3008     'Dly 5.0
3009     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3010     Dly 1.0
3011     M_Out(12340) = 0
3012     '
3013     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3014     M_Out(12835) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3015    Wait M_In(11843) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3016     M_Out(12835) = 0                         '�˂����{�g���N�`�F�b�N��ʐؑ֊���
3017     '
3018     '
3019     MLoopFlg = 1
3020     While MLoopFlg = 1
3021         '
3022         Mov PInitialPosition              '�����ʒu�ړ�
3023         '
3024         MKeyNumber = fnKEY_WAIT()
3025         Select MKeyNumber
3026             Case Is = 1           '��~
3027                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3028                 Dly 1.0
3029                 M_Out(12343) = 0
3030                 Ovrd 20
3031                 'Mov PTicketRead_1
3032                 M_Out(12840) = 1          '�g���N�`�F�b�N�I��
3033                 Wait M_In(11859) = 1      '�˂����{����̏I��
3034                 M_Out(12840) = 0          '�g���N�`�F�b�N�I��
3035                 Ovrd 100
3036                 M_20# = 1
3037                 MLoopFlg = -1
3038                 Break
3039             Case Is = 2           '����
3040                 Break
3041             Case Is = 3           '�p��
3042                 Break
3043             Case Is = 4           '�g���N�`�F�b�N�J�n
3044                 M_Out(12342) = 1          '�g���N�`�F�b�N�J�n�v����M M6342
3045                 Dly 1.0
3046                 M_Out(12342) = 0
3047                 If M_In(11862) = 1 Then             '�g���N�`�F�b�J�[�m�F
3048                     fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3049                     MRet = fnScrewMTorque()           '�˂����{�p�g���N�`�F�b�N
3050                 EndIf
3051                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3052                 'MRet = fnMoveTorquePosi()
3053                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3054                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3055                 Break
3056             Default
3057                 Break
3058         End Select
3059     WEnd
3060     '
3061     '�g���N�`�F�b�N����~���M
3062     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3063     '
3064     '���{�b�g�̈ʒu�����ɖ߂�
3065     '
3066     '
3067  FEnd
3068  '
3069 '
3070 '
3071 '---------------------------
3072 '
3073 '    ���C����ʂ̕\���A��\���ݒ�
3074 '         �R�����gD1001, D1002, D1003�̐ݒ�
3075 '           MWindReSet = 0     ��ʔ�\��
3076 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3077 '           MWindErrScr = 10    �G���[��� D1001, D1002
3078 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3079 '
3080 '---------------------------
3081 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3082     fnMainScreenOpen = 0
3083     '
3084    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3085         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3086     EndIf
3087     '
3088     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3089         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3090     EndIf
3091     '
3092     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3093         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3094     EndIf
3095     '
3096     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3097     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3098     Dly 0.5
3099     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3100 FEnd
3101 '
3102 '��Main
3103 ''' <summary>
3104 ''' �g���N�`�F�b�N������
3105 ''' </summary>
3106 ''' <remarks>
3107 ''' Date   : 2021/12/21 : H.AJI
3108 ''' </remarks>'
3109 Function M% fnScrewMTorque
3110     fnScrewMTorque = 0
3111     M_Out(12838) = 1                         '�g���N�`�F�b�N�J�n1
3112     Wait M_In(11857) = 1                     '��M����
3113     M_Out(12838) = 0                         '�g���N�`�F�b�N�J�n1
3114     Dly 2.0
3115 FEnd
3116 '
3117 '
3118 '----------------------------------------------------------------
3119 'fTimeOutJudge
3120 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3121 '����
3122 'Address% = �Ď��A�h���X�ԍ�
3123 'JudgeFlg% = �ΏۃA�h���X�̐���I�����̒l
3124 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3125 '�߂�l = 0 �G���[
3126 '         1 ����I��
3127 '         2 ���g���C
3128 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3129 '�쐬��
3130 '2022/9/20 ����
3131 '----------------------------------------------------------------
3132 '
3133 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3134     fTimeOutJudge = 0
3135     MJudge% = 1
3136     MRtn = 0
3137     M_20# = MClear%
3138     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3139 *TimeOutLoop
3140     If MRtn = 1 Then GoTo *TimeOut
3141         fErrorProcess(11,202,203,0)
3142         If M_20# = MNext% Then GoTo *TimeOutLoop
3143         If M_20# = MContinue% Then MJudge% = 2
3144         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3145 *TimeOut
3146     fTimeOutJudge = MJudge%
3147 '
3148 *JUDGE_ERROR_END
3149 FEnd
3150 '��Main
3151 ''' <summary>
3152 ''' �g������p�̃��C��
3153 ''' </summary>
3154 ''' <remarks>
3155 ''' Date   : 2021/07/07 : M.Hayakawa
3156 ''' </remarks>'
3157 Function Main
3158     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3159     '
3160     If M_Svo=0 Then
3161         Servo On
3162     EndIf
3163     Wait M_Svo=1
3164 '�g���X�^�[�g���t�����v���p���XON
3165     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3166 '�p�g���C�g����
3167     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3168     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3169     '
3170     M_20# = 0                                   'KEY���͏�����
3171     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
3172     MRet% = 0
3173 '�����ʒu�̊m�F�ƈړ�
3174 '
3175 '���A����@���s�E�����s����      2022/04/08 �n�� �쐬
3176     PActive = P_Curr                    '���݈ʒu���擾
3177     MRecoveryPass% = 0
3178     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3179         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3180             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3181                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3182             EndIf
3183         EndIf
3184     EndIf
3185     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3186         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3187             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3188                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3189             EndIf
3190         EndIf
3191     EndIf
3192     If MRecoveryPass% = 0 Then
3193        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3194     EndIf
3195 '
3196     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3197         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3198 '�g���N�`�F�b�N
3199         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3200             MRet% = fnTorqueCheck()
3201             Break
3202         Else
3203 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
3204 '                MRtn = InspInit()               '�摜��������������
3205 '            EndIf
3206 '
3207             M_20# = MClear%             '������
3208 '�g���J�n
3209             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3210                 fnAssyStart()
3211             Else
3212                 M_20# = MPass%
3213             EndIf
3214 '�g���I�����t����
3215             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3216             Wait M_In(11572) = 1            '���t�擾����
3217             Dly 0.1
3218             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3219 '���t�^�[���j�b�g�ւ�OUT
3220             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3221             fnAutoScreenComment(89)         'AUTO��� �g����������
3222             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3223 'OK/NG�t���O�o��
3224             If M_20# <= 0 Then
3225                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3226             ElseIf M_20# = MPass% Then
3227                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3228             EndIf
3229 'PIAS�ɑg������������
3230             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3231                 If M_20# = MPass% Then
3232                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3233                 Else
3234                     'KEY���͂�NG�̏ꍇ
3235                     If M_20# = MNgProcess% Then
3236                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3237                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3238                         MRet% = fnPiasWrite(MNG%)
3239                        nAssyNgQty = nAssyNgQty + 1
3240                     EndIf
3241                     '
3242                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3243                     If M_20# = MAssyOK% Then
3244                             '-----------------------
3245                             'D732 -> D2600 �R�s�[�v��
3246                             M_Out(12566) = 1
3247 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3248                             M_Out(12566) = 0
3249                             '
3250                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3251                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3252                             '��ԍ��ƍ�(PP�͖��g�p�j
3253 '                            MRet% = fnPCBNumberCheck()
3254                         Else
3255                             MRet% = 1
3256                         EndIf
3257                         '
3258                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3259                             If M_20# <> MAbout% Then
3260                                 '�H������OK��������
3261                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3262                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3263                                 MRet% = fnPiasWrite(MOK%)
3264                                 nAssyOkQty = 0
3265                                 nAssyOkQty = nAssyOkQty + 1
3266                             Else
3267                                 nAssyOkQty = nAssyOkQty + 1
3268                             EndIf
3269                         EndIf
3270                     EndIf
3271 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3272 '                    MRet% = fnPiasWrite(MOK%)
3273                 EndIf
3274             Else
3275                 nAssyOkQty = nAssyOkQty + 1
3276             EndIf
3277             '
3278             '�g���I�����t��������
3279             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3280             '�������A�g��OK���A�g��NG��������
3281 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3282             '
3283 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3284 '                '�摜�����I������
3285 '                MRtn = InspQuit()
3286 '            EndIf
3287         EndIf
3288         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3289     EndIf
3290 '�p�g���C�g����
3291     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3292     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3293 'GOT�\��
3294     fnAutoScreenComment(93)  'AUTO��� �H������
3295 FEnd
3296 End
3297 '
3298 '���܂��Ȃ��R�����g
3299 '��΍폜�����
3300 '
3301 '
3302 '
3303 '
3304 '
3305 '
3306 '
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
PTemp=(+602.00,-150.00,+550.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
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
PActive=(+602.00,-150.00,+550.00,-180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
Pmove=(-17.62,+286.22,+640.00,-179.82,-0.29,+90.49,+0.00,+0.00)(7,1048576)
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
JActive=(+93.47,-14.83,+113.09,+0.17,+81.44,+182.96,+0.00,+0.00)
Jmove=(+93.47,-46.87,+111.64,+0.00,+80.58,+182.96,+0.00,+0.00)
JTaihi=(+0.00,-46.87,+111.64,+0.00,+80.58,+0.00,+0.00,+0.00)
