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
844     Mov PPlateBackSet_13        '�w�ʔu�����
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
871     Mov PPlateBackSet_12        '�ܓ���O���_
872     Cnt 0
873     Ovrd 25
874     Accel 25 , 25
875     Mvs PPlateBackSet_11        '�ܓ��ꍞ�ݑO
876     Mvs PPlateBackSet_10        '�ܓ��ꍞ��1
877     Mvs PPlateBackSet_9         '�ܓ��ꍞ��2
878 '    CmpG 1.0,1.0,1.0,1.0,1.0,1.0,,
879 '    Cmp Pos, &B001000
880     Cnt 1           '�R�����g�A�E�g
881     Mov PPlateBackSet_8         '�o�H1
882     Mov PPlateBackSet_7         '�o�H2
883     Mov PPlateBackSet_6         '�o�H3
884     Mov PPlateBackSet_5         '�o�H4
885     Mov PPlateBackSet_4         '�o�H5
886     Mov PPlateBackSet_3         '�o�H6
887     Mov PPlateBackSet_2         '�o�H7
888     Mov PPlateBackSet_1         '�o�H8
889     Mov PPlateBackSet           '�w�ʔ����ʒu
890 '    Cmp Off
891     Accel 100 , 100
892     Cnt 0
893     Dly 0.1
894 *RE_PLATE_SET
895     M_Out(12256) = 0            '�{�̃`���b�N��OFF
896     M_Out(12257) = 1            '�{�̃`���b�N�JON
897     '
898 '    Wait M_In(11265)            '�{�̃`���b�N�J���o
899     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
900     If MRtn = 1 Then GoTo *CompPlateSet
901     fErrorProcess(11,244,284,0)
902     If M_20# = MNext% Then M_20# = MClear%
903     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
904     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
905     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
906     *CompPlateSet
907 '-----�b�艟��-------------------------------------(22/12/14����)��������
908 *RE_BUCK_PUSH
909     M_20# = MClear%
910     Mov PPlateBackPush_2
911 '
912     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
913     M_Out(12256) = 1            '�{�̃`���b�N��ON
914 '
915     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
916 '
917     If MRtn = 1 Then GoTo *CompBuckPushSetting  '����Ȃ牟�������
918 '
919     fErrorProcess(11,245,287,0) '�[�Z���T�[NG���G���[�\��
920         If M_20# = MNext% Then M_20# = MClear%              '���ւ������ꂽ��l��������
921         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '��~�������ꂽ��G���[�G���h��
922         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END    'NG�������ꂽ��G���[�G���h��
923         If M_20# = MContinue% Then GoTo *RE_BUCK_PUSH       '���g���C�������ꂽ�������x����
924 '
925 *CompBuckPushSetting
926 '
927     Mvs PPlateBackPush_1
928     Ovrd 10
929     Mvs PPlateBackPush
930 '    Dly 0.1     '�N�����v������̂ō폜(221219����)
931 '�w�ʃN�����v��������(12/15)
932     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`��~4)
933     MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
934         If MRtn = 0 Then
935             Mvs PPlateBackPush_1
936             Mov PPlateBackSet_13
937             Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
938         EndIf
939         If MRtn = 0 Then GoTo *ASSY_ERROR_END
940 '�w�ʃN�����v�����܂�(12/15)
941     Ovrd 50                     '20��50�ɕύX(221219����)
942     Mvs PPlateBackPush_1
943 *RE_CHUCK_OPEN
944     M_20# = MClear%
945     M_Out(12256) = 0            '�{�̃`���b�N��OFF
946     M_Out(12257) = 1            '�{�̃`���b�N�JON
947     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
948     If MRtn = 1 Then GoTo *CompChuckOpenForBackPush
949     fErrorProcess(11,244,284,0)
950         If M_20# = MNext% Then M_20# = MClear%              '���ւ������ꂽ��l��������
951         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '��~�������ꂽ��G���[�G���h��
952         If M_20# = MNgProcess% Then GoTo *RE_CHUCK_OPEN    'NG�������ꂽ��G���[�G���h��
953         If M_20# = MContinue% Then GoTo *RE_CHUCK_OPEN       '���g���C�������ꂽ�������x����
954 *CompChuckOpenForBackPush
955 '-----�b�艟��-------------------------------------(22/12/14����)�����܂�
956 '
957     ColChk On
958     Mov PPlateBackSet_13        '�w�ʔu�����
959     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`��~4)
960     Ovrd 100
961 '
962     '
963 ''    ' ���i�����v�����M(�����ʒu�ύX2/27����)
964 '    M_Out(12787) = 1
965     '�˂����{���i�N�����v�Œ�҂�
966 '    Wait M_In(11891) = 1        '�˂����{2��~4��M
967 MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
968 'If MRtn = 0 Then Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
969 If MRtn = 0 Then GoTo *ASSY_ERROR_END
970     '�u���ʒu�摜����
971 '    Mov PPlateBackCheck_3       '�摜�����ʒu���
972 '    Mov PPlateBackCheck_2       '�ʉߓ_
973 '    Mvs PPlateBackCheck         '�m�F�ʒu
974     '
975     'PInspPosition(2) = PPlateBackCheck
976     'MInspGroup(2) = 2
977     'MRtn = ISInspection(PInspPosition,MInspGroup,2,-1,1)
978     'If MRtn <> 1 Then
979     '   '�G���[����
980     'EndIf
981     '
982 ''    ' ���i�����v�����M
983 '    M_Out(12787) = 1    '�����ʒu�ύX
984 '    Mov PPlateBackCheck_3       '�摜�����ʒu���
985     '
986     '�˂����{�������ݑ҂�
987     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`����)
988     '
989     'DVD���J����� CD�ɔ����폜 2022/07/27 M.H
990 *Loop_CW_CCW_S
991     *RE_MECHA_SET_1
992 MRtn = fScrewTighenRoboCheck(11876)    '��~��Ԃ���M����
993 'If MRtn = 0 Then Mov PInitialPosition   '"�C�j�V�����ɖ߂铮��"
994 If MRtn = 0 Then GoTo *ASSY_ERROR_END
995 '
996     *CompRoboGet1
997     '
998 '    Ovrd 50
999     Mov PProductOnRoboGet_3     '�˂����{���i�������_2����3��
1000 '    Ovrd 20
1001     Mvs PProductOnRoboGet_2     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����2��
1002     Ovrd 20
1003     Mvs PProductOnRoboGet_1     '�˂����{���i�����
1004     Ovrd 10
1005     Mvs PProductOnRoboGet       '�˂����{���i���ʒu
1006     Dly 0.2
1007 '
1008     *RE_ROBO_GET_2
1009 '
1010     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1011     M_Out(12256) = 1            '�{�̃`���b�N��ON
1012 '
1013 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
1014     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
1015     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
1016     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1017     M_Out(12257) = 1            '�{�̃`���b�N�JON
1018     Dly 2.0
1019     Mvs PProductOnRoboGet_1
1020     Mvs PProductOnRoboGet_2
1021     Mov PProductOnRoboGet_3
1022     Mov PProductOnRoboGet_4
1023     Mov PInitialPosition
1024     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1025     M_Out(12256) = 1            '�{�̃`���b�N��ON
1026     Dly 1.0
1027     fErrorProcess(11,245,284,0)
1028     If M_20# = MNext% Then MRtn = 1
1029     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1030     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1031     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1032     M_Out(12257) = 1            '�{�̃`���b�N�JON
1033     Dly 2.0
1034     Mov PProductOnRoboGet_4
1035     Mov PProductOnRoboGet_3
1036     Mov PProductOnRoboGet_2
1037     Mvs PProductOnRoboGet_1
1038     Mvs PProductOnRoboGet
1039     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
1040     *CompRoboGet2
1041     M_20# = MClear%
1042     '
1043     Dly 0.2
1044     Mvs PProductOnRoboGet_1     '�˂����{���i�����
1045     Ovrd 50
1046     Mvs PProductOnRoboGet_2     '�˂����{���i�������_(9/27�b��R�����g�A�E�g)12/15�R�����g����
1047     Mvs PProductOnRoboGet_3     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����3��
1048     Ovrd 100
1049     Mov PProductOnRoboGet_4     '�o�H3����4��
1050     Cnt 1 , 10 , 10
1051 '
1052     M_Out(12868) = 1 Dly 0.3    '�˂����{2���슮���𑗐M
1053     *RE_ROBO_GET_3
1054 ' CD�ɔ����폜 2022/07/27 M.H
1055 '    M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1056 '    M_Out(12259) = 1            'DVD���J�`���b�N�JON
1057 ''    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1058 '    MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1059 '    If MRtn = 1 Then GoTo *CompRoboGet3
1060 '    fErrorProcess(11,270,284,0)
1061 '    If M_20# = MNext% Then M_20# = MClear%
1062 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1063 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1064 '    If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1065     *CompRoboGet3
1066     '
1067     '�p���b�g�֐��i��u��
1068     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1069     Cnt 1 , 10
1070     Mov PProductOnPltSet_1      '�{�̒u���ʒu���
1071     Cnt 0
1072     Ovrd 10
1073     Mvs PProductOnPltSet        '�{�̒u���ʒu
1074     Dly 0.5
1075 '
1076     *RE_PLT_SET
1077 '
1078     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1079     M_Out(12257) = 1            '�{�̃`���b�N�JON
1080 '
1081     Wait M_In(11265) = 1        '�{�̃`���b�N�J���o
1082 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1083     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1084     If MRtn = 1 Then GoTo *CompPltSet
1085     fErrorProcess(11,244,284,0)
1086     If M_20# = MNext% Then M_20# = MClear%
1087     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1088         Mvs PProductOnPltSet_1
1089         Mov PProductOnPltSet_2
1090         Mov PInitialPosition
1091     EndIf
1092     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1093     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1094     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1095     *CompPltSet
1096 '
1097     Mvs PProductOnPltSet_1      '�{�̒u���ʒu���
1098     Ovrd 100
1099     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1100 '    Mov PInitialPosition        '�C�j�V�����|�W�V����
1101     MRtn = FnCtlValue2(2)          '�g���n�j�{�P  2022/04/28 �n��
1102     Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
1103     MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1104     '
1105     '�`�P�b�gID��������
1106     M_20# = MAssyOK%
1107     *ASSY_ERROR_END
1108     *AssyEnd
1109     *fnAssyStart_FEndPosi
1110 FEnd
1111 '
1112 '��fnPiasCheck
1113 ''' <summary>
1114 ''' PIAS�`�P�b�g�Ǎ���
1115 ''' </summary>
1116 ''' <returns>   0 : NG
1117 '''             1 : OK(�Ǎ��݊���)
1118 ''' </returns>
1119 ''' <remarks>
1120 ''' Date   : 2021/07/07 : M.Hayakawa
1121 ''' </remarks>'
1122 Function M% fnPiasCheck
1123     fnPiasCheck = 0
1124     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1125     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1126 '
1127 *RETRY_PIAS
1128     M_20# = MClear%
1129     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1130     '
1131     '�yID�`�P�b�g�ǂݍ��݁z
1132     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1133     MInspGroup%(1) = 1              '����G�ԍ�
1134     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1135 '
1136     '�G���[�̏ꍇ
1137     If MRtn <> 1 Then
1138         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1139         If MRtn <> 1 Then
1140             'D720 -> D1300 �R�s�[�v��
1141             M_Out(12565) = 1
1142             Dly 0.5
1143             M_Out(12565) = 0
1144             '�G���[�����L�q
1145             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1146             'GOT KEY���͑҂�
1147             MKeyNumber = fnKEY_WAIT()
1148             '
1149             Select MKeyNumber
1150                 Case MNext%         '���ւ�I�������ꍇ
1151                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1152                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1153                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1154                     Break
1155                 Case MAbout%        '��~��I�������ꍇ
1156                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1157                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1158                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1159                     Break
1160                 Case MNgProcess%    'NG��I�������ꍇ
1161                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1162                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1163                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1164                     Break
1165                 Case MContinue%     '�p����I�������ꍇ
1166                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1167                     M_20# = MContinue%
1168                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1169                     Break
1170             End Select
1171         EndIf
1172     EndIf
1173 '----------D720 -> D1300 �R�s�[�v��----------
1174     M_Out(12565) = 1
1175     Dly 0.5
1176     M_Out(12565) = 0
1177 '----------�ʐM�m�F������----------
1178     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1179     MRtn = 0                ' ������
1180     M_20# = MClear%         ' ������
1181     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1182     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1183     If MRtn <> 1 Then
1184         If M_20# = MContinue% Then
1185             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1186         Else
1187             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1188         EndIf
1189     EndIf
1190 '----------�H�������m�F----------
1191     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1192     MRtn = 0                ' ������
1193     M_20# = MClear%         ' ������
1194     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1195     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1196     If MRtn <> 1 Then
1197         If M_20# = MContinue% Then
1198             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1199         Else
1200             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1201         EndIf
1202     EndIf
1203     '
1204     fnPiasCheck = 1
1205     *fnPiasCheck_End
1206 FEnd
1207 '
1208 '��fnPCComuCheck
1209 ''' <summary>
1210 ''' PC-PLC�ʐM�`�F�b�N
1211 ''' </summary>
1212 ''' <returns>   0 : NG
1213 '''             1 : OK(�Ǎ��݊���)
1214 ''' </returns>
1215 ''' <remarks>
1216 ''' Date   : 2021/07/07 : M.Hayakawa
1217 ''' </remarks>'
1218 Function M% fnPCComuCheck
1219     fnPCComuCheck = 0
1220     MJudge% = 0                                  '������
1221     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1222     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1223     '
1224     For MStaNo = 0 To 5
1225         '
1226         If M_In(MIN_PIAS_ComOK%) = 1 Then
1227             'PC�ʐMOK(M400)
1228             MJudge% = MOK%
1229             MStaNo = 5
1230             Break
1231         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1232             'toRBT_�ʐM�m�Ftime out
1233             MJudge% = MNG%
1234             MCommentD1001 = 15
1235             MCommentD1002 = 21
1236             MStaNo = 5
1237             Break
1238         Else
1239             'toRBT_�ʐM�m�Ftime out
1240             MJudge% = MNG%
1241             MCommentD1001 = 14
1242             MCommentD1002 = 21
1243             Break
1244         EndIf
1245     Next MStaNo
1246     '
1247     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1248     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1249     '
1250     '�G���[���
1251     If MJudge% <> MOK% Then
1252         M_20# = MClear%     '������
1253         '�G���[�����L�q
1254         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1255         'GOT KEY���͑҂�
1256         MKeyNumber = fnKEY_WAIT()
1257         '
1258         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1259             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1260             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1261             Break
1262         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1263             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1264             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1265             Break
1266         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1267             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1268             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1269             Break
1270         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1271             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1272             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1273             Break
1274         EndIf
1275     Else
1276         'OK�̏ꍇ
1277         fnPCComuCheck = 1
1278     EndIf
1279 FEnd
1280 '
1281 '��fnProcessCheck
1282 ''' <summary>
1283 ''' �H�������m�F
1284 ''' </summary>
1285 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1286 '''             -1�F�O�H������NG  -2�F���H����������
1287 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1288 '''             -5�F���������G���[
1289 ''' </returns>
1290 ''' <remarks>
1291 ''' Date   : 2021/07/07 : M.Hayakawa
1292 ''' </remarks>'
1293 Function M% fnProcessCheck
1294     fnProcessCheck = 0
1295     MJudge% = MNG%      '��UNG���������Ƃ���
1296 '----------�H�������m�F----------
1297     MCommentD1001 = 0   '�R�����g������
1298     For MStaNo = 0 To 5
1299         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1300         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1301         '
1302         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1303             MJudge% = MOK%
1304             fnAutoScreenComment(85)     ' AUTO���
1305             MStaNo = 5
1306             Break
1307         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1308             MFlgLoop% = 0
1309             MJudge% = MNG%
1310             MCommentD1001 = 27
1311             MCommentD1002 = 22
1312             fnAutoScreenComment(94)     ' AUTO���
1313             fnProcessCheck = -2         ' NG��-2��Ԃ�
1314             MStaNo = 5
1315             Break
1316         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1317            MJudge% = MNG%
1318             MCommentD1001 = 31
1319             MCommentD1002 = 22
1320             fnAutoScreenComment(83)     ' AUTO���
1321             fnProcessCheck = -3         ' NG��-3��Ԃ�
1322             MStaNo = 5
1323             Break
1324         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1325             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1326             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1327             MJudge% = MNG%
1328             MCommentD1001 = 32
1329             MCommentD1002 = 22
1330             fnAutoScreenComment(84)     ' AUTO���
1331             fnProcessCheck = -1         ' NG��-1��Ԃ�
1332             Dly 1.0
1333             '�H�������m�FOFF
1334             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1335             Dly 1.0
1336            'MStaNo = 5
1337             Break
1338         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1339             MFlgLoop% = 0
1340             MJudge% = MNG%
1341             MCommentD1001 = 29
1342             MCommentD1002 = 22
1343             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1344             fnProcessCheck = -5         ' NG��-5��Ԃ�
1345             MStaNo = 5
1346             Break
1347         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1348             MJudge% = MNG%
1349             If MCommentD1001 = 32 Then
1350                 '�������Ȃ�
1351             Else
1352                 MCommentD1001 = 26
1353             EndIf
1354             MCommentD1002 = 22
1355             fnProcessCheck = -4         ' NG��-4��Ԃ�
1356             MStaNo = 5
1357             Break
1358         Else
1359             MJudge% = MNG%
1360             MCommentD1001 = 28
1361             MCommentD1002 = 22
1362         EndIf
1363     Next MStaNo
1364     '�H�������m�FOFF
1365     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1366     '�ʉߗ���NG �H�������̏ꍇ
1367     If MJudge% = MPass% Then
1368         M_20# = MPass%
1369     EndIf
1370     '
1371     '�G���[���
1372     If MJudge% <> MOK% Then
1373         M_20# = MClear%     '������
1374         '�G���[�����L�q
1375         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1376         'GOT KEY���͑҂�
1377         MKeyNumber = fnKEY_WAIT()
1378         '
1379         Select MKeyNumber
1380             Case MAbout%        '��~��I�������ꍇ
1381                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1382                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1383                 Break
1384             Case MNext%         '���ւ�I�������ꍇ
1385                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1386                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1387                 Break
1388             Case MContinue%     '�p����I�������ꍇ
1389                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1390                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1391                 Break
1392             Case MNgProcess%    'NG��I�������ꍇ
1393                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1394                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1395                 Break
1396         End Select
1397     Else
1398         fnProcessCheck = 1  ' OK��1��Ԃ�
1399     EndIf
1400 FEnd
1401 '
1402 '��fnPiasWrite
1403 ''' <summary>
1404 ''' Pias �g�����ʏ����ݗv��
1405 ''' </summary>
1406 '''<param name="MFlg%">
1407 '''                 MOK%(1) = �H��������OK��������
1408 '''                 MNG%(0) = �H��������NG��������
1409 '''</param>
1410 '''<returns></returns>
1411 ''' <remarks>
1412 ''' Date   : 2021/07/07 : M.Hayakawa
1413 ''' </remarks>'
1414 Function M% fnPiasWrite(ByVal MFlg%)
1415       fnPiasWrite = 0
1416 *RETRY_PIASWRITE
1417     '
1418     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1419    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1420     If MFlg% = MOK% Then
1421         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1422     Else
1423         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1424     EndIf
1425     Dly 0.1                  '�O�̂���
1426     '
1427     'Pias�֏����݊J�n M305 -> ON
1428     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1429     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1430     '
1431     MJudge% = MNG%
1432     '
1433     For MStaNo = 0 To 5
1434         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1435             MJudge% = MOK%
1436             'MRet = fnAutoScreenComment(85)  'AUTO���
1437             MStaNo = 5
1438             Break
1439         '
1440         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1441             MJudge% = MNG%
1442             'MRet = fnAutoScreenComment(85)  'AUTO���
1443            MCommentD1001 = 34
1444            MCommentD1002 = 25
1445             MStaNo = 5
1446             Break
1447         '
1448         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1449             MJudge% = MNG%
1450             'MRet = fnAutoScreenComment(85)  'AUTO���
1451            MCommentD1001 = 35
1452            MCommentD1002 = 25
1453             MStaNo = 5
1454             Break
1455         '
1456         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1457             MJudge% = MNG%
1458             'MRet = fnAutoScreenComment(85)  'AUTO���
1459            MCommentD1001 = 36
1460            MCommentD1002 = 25
1461             MStaNo = 5
1462             Break
1463         '
1464         Else
1465             MJudge% = MNG%
1466            MCommentD1001 = 42
1467            MCommentD1002 = 25
1468         '
1469         EndIf
1470         '
1471     Next MStaNo
1472     '
1473     'Pias�֏����݊J�n M305 -> OfF
1474     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1475     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1476     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1477     '
1478     '
1479     '�ʉߗ���NG �H�������̏ꍇ
1480     If MJudge% = MPass% Then
1481         M_20# = MPass%
1482     EndIf
1483     '
1484    M_20# = MClear%     '������
1485     '
1486     '�G���[���
1487     If MJudge% < MOK% Then
1488     '
1489 '�c���Ă���������ł͎g�p���Ȃ����x��
1490 *RETRY_ERR_WRITE
1491         M_20# = MClear%     '������
1492         '�G���[�����L�q
1493         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1494         'GOT KEY���͑҂�
1495         MKeyNumber = fnKEY_WAIT()
1496         '
1497         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1498             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1499            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1500             Break
1501         '
1502         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1503             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1504             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1505         '
1506         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1507             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1508             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1509         '
1510         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1511             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1512            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1513             Break
1514         '
1515         EndIf
1516         '
1517         If M_20# = MClear% Then *RETRY_ERR_WRITE
1518         '
1519     EndIf
1520     '
1521     If M_20# = MContinue% Then *RETRY_PIASWRITE
1522     '
1523     fnPiasWrite = 1
1524     '
1525 FEnd
1526 '
1527 '��fnPCBNumberCheck
1528 ''' <summary>
1529 ''' Pias ��ԍ��ƍ��v��
1530 ''' </summary>
1531 '''<param name="%"></param>
1532 '''<param name="%"></param>
1533 '''<returns></returns>
1534 ''' <remarks>
1535 ''' Date   : 2021/07/07 : M.Hayakawa
1536 ''' </remarks>'
1537 Function M% fnPCBNumberCheck
1538       fnPCBNumberCheck = 0
1539     '
1540 *RETRY_PCBCHECK
1541     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1542     'Pias�֊�ƍ��J�n M310 -> ON
1543     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1544     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1545     '
1546     MJudge% = MNG%
1547     '
1548     For MStaNo = 0 To 5
1549         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1550             MJudge% = MOK%
1551             fnAutoScreenComment(96)  'AUTO���
1552             MStaNo = 5
1553             Break
1554         '
1555         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1556             MJudge% = MNG%
1557             fnAutoScreenComment(97)  'AUTO���
1558             MCommentD1001 = 37
1559             MCommentD1002 = 25
1560             MStaNo = 5
1561             Break
1562         '
1563         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1564             MJudge% = MNG%
1565             fnAutoScreenComment(98)  'AUTO���
1566             MCommentD1001 = 38
1567             MCommentD1002 = 25
1568             MStaNo = 5
1569             Break
1570         '
1571         ElseIf M_In(11580) = 1 Then                         'time out
1572             MJudge% = MNG%
1573             fnAutoScreenComment(99)  'AUTO���
1574             MCommentD1001 = 39
1575             MCommentD1002 = 25
1576             MStaNo = 5
1577             Break
1578         '
1579         Else
1580             MJudge% = MNG%
1581            MCommentD1001 = 41
1582            MCommentD1002 = 25
1583         '
1584         EndIf
1585         '
1586     Next MStaNo
1587     '
1588     'Pias�֊�ƍ��J�n M310 -> OfF
1589     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1590     '
1591     '
1592     '�ʉߗ���NG �H�������̏ꍇ
1593     If MJudge% = MPass% Then
1594         M_20# = MPass%
1595     EndIf
1596     '
1597    M_20# = MClear%     '������
1598     '
1599     '�G���[���
1600     If MJudge% < MOK% Then
1601     '
1602 '�c���Ă���������ł͎g�p���Ȃ����x��
1603 *RETRY_ERR_PCBNUMBER
1604         M_20# = MClear%     '������
1605         '�G���[�����L�q
1606         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1607         'GOT KEY���͑҂�
1608         MKeyNumber = fnKEY_WAIT()
1609         '
1610         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1611             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1612             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1613             Break
1614         '
1615         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1616             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1617             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1618         '
1619         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1620             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1621             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1622         '
1623         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1624             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1625             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1626             Break
1627         '
1628         EndIf
1629         '
1630         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1631         '
1632     EndIf
1633     '
1634     If M_20# = MContinue% Then *RETRY_PCBCHECK
1635 FEnd
1636 '
1637 '��ScrewTight_S2
1638 ''' <summary>
1639 ''' �˂����߂��s��
1640 ''' </summary>
1641 '''<param name="PScrewPos()">
1642 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1643 '''             PScrewPos(2)    �F�˂����߉��_
1644 '''             PScrewPos(10)   �F�˂����ߏI������
1645 '''</param>
1646 '''<returns>����
1647 '''         0=�ُ�I���A1=����I��
1648 '''</returns>
1649 ''' <remarks>
1650 ''' Date   : 2021/07/07 : M.Hayakawa
1651 ''' </remarks>'
1652 Function M% ScrewTight_S2(ByVal PScrewPosition())   '�l�W���ߌʐݒ�
1653     ScrewTight_S2 = 0
1654     MOKNGFlg = 0
1655     Ovrd 100
1656     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1657     ' �b��
1658     Ovrd 5
1659     Mvs PScrewPosition(10),-10    ' �p���b�g��˂�����S�@�̏��ֈړ�
1660 '    Ovrd MOvrdA
1661     '�b��}�X�N
1662 '    M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
1663 '    Dly 0.1
1664 '    M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
1665 '    'Spd 8.3 '�O�����C�h100  �������C�h60   '���C�h100-40�@100%�FSpd�@15�@'�˂����ߑ��x�ݒ�
1666 '    Spd MSpdA               '�l�W���ߎ�Spd�ʐݒ�
1667     ' �b��ړ��̂�
1668     Mvs PScrewPosition(10)
1669 '    '
1670 '    Dly 0.1
1671 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1672 '    Wait M_In(11584)=1          '����/�G���[���o
1673 '    Dly 0.1
1674 '    Spd M_NSpd
1675 '    '
1676 '    If M_In(X28_Driver)=1 Then  '�˂��g�[�^���G���[���o��
1677 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1678 '        Dly 0.1
1679 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1680 '        Dly 0.1
1681 '        M_Out(Y63_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
1682 '        Dly 0.1
1683 '        M_Out(Y65_Driver)=0     '�v���O���������@F1
1684 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1685 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
1686 '        MOKNGFlg = -1
1687 '        ScrewTight_S2 = 0
1688 '    Else
1689 '        Wait M_In(X29_Driver)=1 ' ���튮����
1690 '        Dly 0.1
1691 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
1692 '        Dly 0.1
1693 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O����
1694 '        Dly 0.1
1695 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
1696 '        Dly 0.1
1697 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
1698 '        ScrewTight_S2 = 1
1699 '    EndIf
1700 ' �b��
1701     Ovrd 10
1702     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1703     Ovrd 100
1704 FEnd
1705 '
1706 '��ScrewGet_S3
1707 ''' <summary>
1708 ''' �˂������@����˂��𓾂�
1709 ''' </summary>
1710 '''<param name="%"></param>
1711 '''         PScrewPos(1)    �F�˂�������̂˂����
1712 '''         PScrewPos(2)    �F�˂���������_
1713 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1714 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1715 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1716 '''<returns>����
1717 '''         0=�ُ�I���A1=����I���A-1=M�l�W�Z���T�[NG�A-2=M�l�W�Z���T�[ON�A-3=�z���G���[
1718 '''</returns>
1719 ''' <remarks>
1720 ''' Date   : 2021/07/07 : M.Hayakawa
1721 ''' </remarks>'
1722 Function M% ScrewGet_S3(ByVal PScrewPosition())
1723     ScrewGet_S3 = 0
1724     MMScrewJudge% = 0
1725     '�˂������평������G���[�`�F�b�N
1726 ' ���b��폜
1727 '    Wait M_In(X34_ScrewReady1)=1 '�˂�������S��Ready�ɂȂ�܂ő҂@���@���b���҂���Ready�ɂȂ�Ȃ���Δ�����v���O�������K�v�H
1728 '    Ovrd 100
1729 '    If M_In(X33_SS2)=0 Then  'M�˂����o�Z���T��OFF�i�̏�j���Ă����ꍇ
1730 '        Ovrd 30
1731 '        Mvs,-80             '���̏ꏊ����80mm���ֈړ�
1732 '        Mov PInitPos19049   '19049�����ʒu�ֈړ�
1733 '        M_Out(Y6A_VV1)=0    '�˂��z�� Off
1734 '        'NG�Ƃ��Ă����̊֐����甲����
1735 '        ScrewGet_S3 = -1
1736 '        MMScrewJudge% = 1
1737 '        MCommentD1001 = 61
1738 '    EndIf
1739 '    If ScrewGet_S3 = 0 Then
1740 '        'S�^�C�g�p�˂������@��M�˂����������Ă��Ȃ����Ď�
1741 '        MMScrewJudge% = 0 'MMScrewJudge������������
1742 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
1743 '        If MRtn = 0 Then
1744 '            Ovrd 30
1745 '            Mvs,-80            '���̏ꏊ����50mm���ֈړ�
1746 '            Mov PInitPos19049  '19049�����ʒu�ֈړ�
1747 '            MMScrewJudge% = 2
1748 '            MRtn = All_CLamp_Release()'�S�ẴN�����v�����֕���
1749 '            MCnt% = 2   '2��ݒ�
1750 '            MCommentD1001 = 62
1751 '        EndIf
1752 '        If MMScrewJudge% = 2 Then
1753 '            ScrewGet_S3 = -2
1754 '        EndIf
1755 '    EndIf
1756 '    'M�l�W���肪ON�̏ꍇ NG�Ƃ��Ċ֐��𔲂���
1757 '    If MMScrewJudge% = 2 Then
1758 '        ScrewGet_S3 = -2
1759 '    EndIf
1760     'S�l�W�p�˂����Y��M�l�W�����m�F�p�����܂�
1761     Ovrd 100
1762     Spd M_NSpd
1763     If MMScrewJudge% = 0 Then
1764         ScrewGet_S3 = 0
1765         M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
1766         MScrewCnt% = 0
1767         MFinCnt% = 2
1768 '        For MCnt% = 0 To MFinCnt%
1769             Mov PScrewPosition(2)        ' �˂������@���_
1770             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
1771             Ovrd 80
1772             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1773             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
1774             Mvs PScrewPosition(10), 1.2
1775             M_Out(Y6A_VV1)=1        ' �˂��z���@ON
1776             '�r�b�g��]
1777             M_Out(Y60_Driver)=1
1778             Dly 0.2
1779             '
1780             Ovrd 100
1781             JOvrd M_NJovrd
1782             Spd M_NSpd
1783             '�l�W�z���m�F�ʒu�ړ�
1784             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
1785             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
1786             '�r�b�g��]��~
1787             'M_Out(Y60_Driver)=0
1788             '
1789             '1�b�ԃl�W�z���m�F
1790 ' �ȉ��b��폜
1791 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1792 '            'MRtn = 0'�����G���[
1793 '            '�z���G���[�̏ꍇ
1794 '            '�l�W���˂����Y�ɖ߂�
1795 '            If MRtn = 0 Then
1796 '                Ovrd 30
1797 '                '�r�b�g��]��~
1798 '                M_Out(Y60_Driver)=0
1799 '                '�l�W�����@���
1800 '                Mvs PScrewPos(1)
1801 '                '�X�ɏ��
1802 '                Mov PScrewPos(1), -75
1803 '                '�l�W�̂Ĉʒu
1804 '                Mov PScrewFeedS021
1805 '                '�z��OFF
1806 '                M_Out(Y6A_VV1)=0 '�˂��z���@OFF
1807 '                Dly 0.2
1808 '                '�j��ON
1809 '                M_Out(Y6B_VB1)=1 '�^��j��ON
1810 '                '�r�b�g��]
1811 '                M_Out(Y61_Driver)=1
1812 '                Dly 0.5
1813 '                '
1814 '                Ovrd 100
1815 '                JOvrd M_NJovrd
1816 '                Spd M_NSpd
1817 '                '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1818 '                Mov PScrewFeedS021, 10
1819 '                Mov PScrewFeedS021
1820 '                Dly 0.1
1821 '                Mov PScrewFeedS021, 10
1822 '                Mov PScrewFeedS021
1823 '                '
1824 '                '�l�W�����҂�
1825 '                '�r�b�g��]��~
1826 '                M_Out(Y61_Driver)=0
1827 '                Dly 0.1
1828 '                '�j��OFF
1829 '                M_Out(Y6B_VB1)=0 '�^��j��OFF
1830 '                '
1831 '                '
1832 '                '�˂��������Ƃ��āA�ړ��X�ɏ��
1833 '                Mov PScrewPos(1), -75
1834 '                Ovrd 100
1835 '                Spd M_NSpd
1836 '                '�l�W�����@���
1837 '                Mvs PScrewPos(1)
1838 '                '
1839 '                ScrewGet_S3 = -3
1840 '                Break
1841 '                '
1842 '            Else
1843 '                MCnt% = MFinCnt%
1844 '                ScrewGet_S3 = 0
1845 '            EndIf
1846 '        Next  MCnt%
1847         '
1848         Ovrd 100
1849         Spd M_NSpd
1850         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
1851         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
1852         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
1853         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
1854         '������x�z���m�F
1855 ' �ȉ��b��폜
1856 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
1857 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
1858 '            MCommentD1001 = 94
1859 '            MCommentD1002 = 95
1860 '            ScrewGet_S3 = -3
1861 '        EndIf
1862 '        If MRtn = 1 Then      '�z��OK�̏ꍇ
1863 '            ScrewGet_S3 = 1
1864 '        EndIf
1865 '        Break
1866     Else
1867         'M�l�W
1868         If MMScrewJudge% = 2 Then
1869             ScrewGet_S3 = -2
1870         EndIf
1871     EndIf
1872 FEnd
1873 '
1874 '��fnKEY_WAIT()
1875 ''' <summary>
1876 ''' GOT����̃L�[���͑҂�
1877 ''' </summary>
1878 '''<returns>1�F��~    2�F����
1879 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
1880 '''         5�FNG
1881 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
1882 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
1883 '''</returns>
1884 ''' <remarks>
1885 ''' Date   : 2021/07/07 : M.Hayakawa
1886 ''' </remarks>'
1887 Function M% fnKEY_WAIT()
1888     fnKEY_WAIT = 0
1889     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
1890     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
1891     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
1892     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1893     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
1894     Dly 0.2
1895     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
1896     MLocalLoopFlg=1
1897     While MLocalLoopFlg=1
1898         If M_In(11345) = 1 Then         '��~   M5345
1899             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
1900             fnKEY_WAIT = 1
1901             MLocalLoopFlg=-1
1902             Break
1903         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
1904             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
1905             fnKEY_WAIT = 2
1906             MLocalLoopFlg=-1
1907             Break
1908         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
1909             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
1910             fnKEY_WAIT = 3
1911             MLocalLoopFlg=-1
1912             Break
1913         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
1914             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
1915             fnKEY_WAIT = 4
1916             MLocalLoopFlg=-1
1917             Break
1918         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
1919             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
1920             fnKEY_WAIT = 5
1921             MLocalLoopFlg=-1
1922             Break
1923             '
1924         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
1925             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
1926             fnKEY_WAIT = MRobotInit1%
1927             MLocalLoopFlg=-1
1928             Break
1929             '
1930         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
1931             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
1932             fnKEY_WAIT = MRobotInit2%
1933             MLocalLoopFlg=-1
1934             Break
1935             '
1936         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
1937             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
1938             fnKEY_WAIT = MRobotInit3%
1939             MLocalLoopFlg=-1
1940             Break
1941             '
1942         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
1943             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
1944             fnKEY_WAIT = MRobotInit4%
1945             MLocalLoopFlg=-1
1946             Break
1947             '
1948         Else
1949         EndIf
1950     WEnd
1951     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
1952     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
1953 FEnd
1954 '
1955 '�� fnAUTO_CTL
1956 ''' <summary>
1957 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
1958 ''' </summary>
1959 ''' <remarks>
1960 ''' Date   : 2021/07/07 : M.Hayakawa
1961 ''' </remarks>
1962 Function M% fnAUTO_CTL
1963     fnAUTO_CTL = 0
1964     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1965     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
1966     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1967     '
1968     If M_Svo=0 Then             '�T�[�{ON�m�F
1969         Servo On
1970     EndIf
1971     Wait M_Svo=1
1972 FEnd
1973 '
1974 '�� fnWindScreenOpen
1975 ''' <summary>
1976 ''' �E�B���h��ʂ̕\���A��\���ݒ�
1977 ''' </summary>
1978 '''<param name="%"></param>
1979 '''<param name="%"></param>
1980 '''<param name="%"></param>
1981 '''<param name="%"></param>
1982 ''' <remarks>
1983 ''' �R�����gD1001, D1002, D1003�̐ݒ�
1984 ''' MWindReSet = 0     ��ʔ�\��
1985 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
1986 ''' MWindErrScr = 10    �G���[��� D1001, D1002
1987 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
1988 ''' Date   : 2021/07/07 : M.Hayakawa
1989 ''' </remarks>
1990 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
1991     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
1992         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
1993     EndIf
1994     '
1995     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
1996         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
1997     EndIf
1998     '
1999     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2000        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2001     EndIf
2002     '
2003     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2004     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2005     Dly 0.5
2006     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2007 FEnd
2008 '
2009 '��FnCtlValue2
2010 ''' <summary>
2011 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2012 ''' </summary>
2013 ''' <param name="MCtlNo%"></param>
2014 ''' <remarks>
2015 ''' Date : 2022/04/28 �n��
2016 ''' </remarks>
2017 '''
2018 '''  1�F������       �{�P
2019 '''  2�F�g���n�j��   �{�P
2020 '''  3�F�g���m�f��   �{�P (���g�p)
2021 '''  4�F�z���G���[�� �{�P
2022 ''' 99�F�Ǐ��J�n�M�� OFF
2023 '''
2024 Function M% FnCtlValue2(ByVal MCtlNo%)
2025     FnCtlValue2 = 1
2026     Select MCtlNo%
2027         Case 1        '�������{�P
2028             M_Out(12569) = 0             '�����݊J�n�M��OFF
2029             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2030             MInputQty = M_In16(11600)    '��������M
2031             MInputQty = MInputQty + 1    '�������{�P
2032             M_Out16(12592) = MInputQty   '���������M
2033             M_Out(12569) = 1             '�����݊J�n�M��ON
2034             Break
2035             '
2036         Case 2        '�g���n�j���{�P
2037             M_Out(12569) = 0             '�����݊J�n�M��OFF
2038             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2039             MAssyOkQty = M_In16(11616)   '�g��OK����M
2040             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2041             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2042             M_Out(12569) = 1             '�����݊J�n�M��ON
2043             Break
2044             '
2045         Case 4        '�z���G���[���{�P
2046             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2047             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2048             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2049             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2050             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2051             M_Out(12569) = 1                       '�����݊J�n�M��ON
2052             Break
2053             '
2054         Case 99        '�Ǐ��J�n�M��OFF
2055             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2056             M_Out(12569) = 0        '�����݊J�n�M��OFF
2057             Break
2058             '
2059     End Select
2060     Exit Function
2061 FEnd
2062 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2063 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2064 '-------------------------------------------------------------------------------
2065 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2066 '   ����
2067 '       PInspPos()      �F�����ʒu
2068 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2069 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2070 '       MInspCnt%       �F�����ʒu��
2071 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2072 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2073 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2074 '   �߂�l�F����
2075 '       0=�ُ�I���A1=����I��
2076 '
2077 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2078 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2079 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2080 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2081 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2082 '-------------------------------------------------------------------------------
2083     '----- �����ݒ� -----
2084     Cnt 0                                                           '�ړ�����������(�����l=0)
2085     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2086 '    Cnt 1,0.1,0.1
2087     '�ϐ��錾�E������
2088     Def Inte MNum                                                   '�����ԍ�(������1�`)
2089     MNum% = 1                                                       '�����ԍ������l�ݒ�
2090     Def Inte MEndFlg                                                '�����I���t���O
2091     MEndFlg% = 0
2092     '
2093     '����G�ԍ��ݒ�v���E�������s�v��off
2094     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2095     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2096     '�G���[�ԍ��N���A
2097     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2098     M_Out16(MOUT_InspErrNum) = MInspErrNum
2099     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2100     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2101     '
2102     'Insight Ready check?
2103     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2104         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2105         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2106         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2107         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2108         Exit Function
2109     EndIf
2110     '
2111     '�����ʒu���m�F
2112     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2113         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2114         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2115         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2116         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2117         Exit Function
2118     EndIf
2119     '
2120     '
2121     '
2122     '----- ���C������ -----
2123     '�ݒ肳�ꂽ�����ʒu�����̌������s
2124     While( MEndFlg% = 0 )
2125         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2126         MSetGrNumRetryExitFlg = 0
2127         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2128         While( MSetGrNumRetryExitFlg = 0 )
2129         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2130             '
2131             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2132             '
2133             '----- �����O���[�v�ԍ��ݒ� -----
2134             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2135             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2136             '
2137             '�����ʒu�ֈړ��E�ړ������҂�
2138             Mvs PInspPos( MNum% )                                       '�ړ�
2139             Dly 0.05                                                    '�ړ�������Delay
2140             '
2141             '�����O���[�v�ԍ��ݒ�I���m�F
2142             M_Timer(1) = 0
2143             MExitFlg = 0
2144             While( MExitFlg = 0 )
2145                 '����G�ݒ萳��I��?
2146                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2147                     MExitFlg = 1
2148                 '
2149                 '����G�ݒ�ُ�I��?
2150                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2151                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2152                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2153                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2154                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2155                     EndIf
2156                     MExitFlg = 1
2157                 '
2158                 'timeout�`�F�b�N
2159                 ElseIf 1000 < M_Timer(1) Then
2160                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2161                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2162                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2163                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2164                     EndIf
2165                     MExitFlg = 1
2166                 EndIf
2167             WEnd
2168             '
2169             '����G�ԍ��ݒ�v��off
2170             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2171             '
2172             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2173             'NG�Ȃ���Δ�����
2174             If MCurrentStepErr = 0 Then
2175                 MSetGrNumRetryExitFlg = 1
2176             Else
2177                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2178                 If MSetGrNumRetryCnt = 0 Then
2179                     MSetGrNumRetryExitFlg = 1
2180                 Else
2181                     'Retry�ց@���̑O��Delay
2182                     Dly 0.5
2183                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2184                 EndIf
2185             EndIf
2186             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2187             '
2188         WEnd
2189         '
2190         '
2191         '
2192         '----- �������s -----
2193         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2194             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2195                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2196                 MInspRetryExitFlg = 0
2197                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2198                 While( MInspRetryExitFlg = 0 )
2199                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2200                     '
2201                     '���������m�F
2202                     MRetryCnt = MRetryCnt - 1
2203                     M_Timer(1) = 0
2204                     MExitFlg = 0
2205                     While( MExitFlg = 0 )
2206                     '���������҂�
2207                         '����OK�I��?
2208                         If M_In( MIN_IS_InspOK% ) = 1  Then
2209                             MJudgeOKFlg = 1                         '����OK�t���OON
2210                             MExitFlg = 1
2211                         '
2212                         '����NG�I��?
2213                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2214                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2215                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2216                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2217                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2218                                 EndIf
2219                             EndIf
2220                             MExitFlg = 1
2221                         '
2222                         '�����ُ�I��(IS timeout)?
2223                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2224                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2225                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2226                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2227                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2228                                 EndIf
2229                             EndIf
2230                             MExitFlg = 1
2231                         '
2232                         'timeout�`�F�b�N
2233                         ElseIf 3000 < M_Timer(1) Then
2234                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2235                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2236                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2237                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2238                                 EndIf
2239                             EndIf
2240                             MExitFlg = 1
2241                         EndIf
2242                     WEnd
2243                     '
2244                     '�����J�n�v��off
2245                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2246                     '
2247                     'OK�Ȃ甲����
2248                     If MJudgeOKFlg = 1 Then
2249                         MInspRetryExitFlg = 1
2250                     Else
2251                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2252                         If MRetryCnt = 0 Then
2253                             MInspRetryExitFlg = 1
2254                         Else
2255                             'Retry�ց@���̑O��Delay
2256                             Dly 0.3
2257                         EndIf
2258                     EndIf
2259                     '
2260                 WEnd
2261             EndIf
2262         EndIf
2263         '
2264         '
2265         '
2266         MNum% = MNum% + 1                                           '����Step+1
2267         '�����I���m�F�@�����I���t���O�Z�b�g
2268         If (MInspCnt% < MNum% ) Then
2269             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2270         EndIf
2271         'NG���������s������
2272         If MInspErrNum <> 0 Then                                    'NG����?
2273             If MNgContinue% <> 1 Then                               'NG���s?
2274                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2275             EndIf
2276         EndIf
2277     WEnd
2278     '
2279     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2280     If 0 < MZAxis% Then
2281         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2282         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2283         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2284     EndIf
2285     Fine 0 , P
2286     '
2287     '�߂�l�ݒ�
2288     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
2289         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2290     Else
2291         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2292         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2293         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2294     EndIf
2295     '
2296 FEnd
2297 '
2298 ' ��ISInspection
2299 ''' <summary>
2300 ''' Insight�ɂ��摜�����������s
2301 ''' </summary>
2302 '''<param name="PInspPos()">�����ʒu</param>
2303 '''<param name="MInspGrNum%()">�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j</param>
2304 '''             PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2305 '''<param name="MInspCnt%">�����ʒu��</param>
2306 '''<param name="MZAxis%">�I������Z���ޔ����W�i-1:�����j</param>
2307 '''             �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2308 '''<param name="MNgContinue%">=1�Ō����G���[�ENG�������ɑSStep�̌������s��</param>
2309 '''<returns>    ���� 0=�ُ�I���A1=����I��</returns>
2310 '''         MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2311 '''         MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���"
2312 ''' <remarks>
2313 ''' Date   : 2021/07/07 : M.Hayakawa
2314 ''' </remarks>
2315 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2316 '    '�摜�g�p�m�F 0<- �摜�m�F�����̏ꍇ
2317 '    If M_In(11369) = 0 Then            'toRBT_�g�p�m�F
2318 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2319 '    EndIf
2320 ''
2321 '    Cnt 0                                                       '�ړ�����������(�����l=0)
2322 '    Fine 0.05,P                                                 '�ʒu���ߊ��������ݒu�@0.05mm
2323 '    MNum% = 1                                                   '�����ԍ������l�ݒ�
2324 '    Def Inte MEndFlg                                            '�����I���t���O
2325 '    MEndFlg% = 0
2326 '    '
2327 '    '�G���[�ԍ��N���A
2328 '    MInspErrNumSub = 0                                          '�������s�G���[�ԍ�sub
2329 '    MInspErrNum = 0                                             '�������s�G���[�ԍ�
2330 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2331 '    MInspNGStepNum = 0                                          '�������sNGStep�ԍ�
2332 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2333 '    '
2334 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready off�Ȃ�I��
2335 '        MInspErrNum = 20                                        '�������s�G���[�ԍ� 20 Insight offline
2336 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2337 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2338 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2339 ''
2340 '    EndIf
2341 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2342 '    '
2343 '    '�����ʒu���m�F
2344 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2345 '        MInspErrNum = 21                                        '�����f�[�^�Ȃ� 21�@����<1
2346 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2347 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2348 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2349 ''
2350 '    EndIf
2351 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2352 '    '
2353 '    '�ݒ肳�ꂽ�����ʒu�����̌������s
2354 '    While( MEndFlg% = 0 )
2355 '        '�����I���m�F�@�����I���t���O�Z�b�g
2356 '        If (MInspCnt% < MNum% ) Then
2357 '            MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2358 '        EndIf
2359 '        '
2360 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����J�n�@INSPTAST1
2361 '        If MEndFlg% = 0 Then
2362 '            M_01# = MInspGrNum%(MNum%)                          '����G�ԍ����n��
2363 '        EndIf
2364 '        M_02# = MEndFlg%                                        '�����I���t���O���n��
2365 '        M_05# = MNum%                                           '�����ԍ�(������1�`)
2366 '        '�^�X�N�@����G�ݒ�t���O���n��
2367 '        If MEndFlg% = 0 Then
2368 '            If 0 < MInspGrNum%(MNum%) Then
2369 '                M_03# = 1
2370 '            Else
2371 '                M_03# = 0
2372 '            EndIf
2373 '        Else
2374 '            M_03# = 0
2375 '        EndIf
2376 '        '�^�X�N�@�������ʊm�F�t���O���n��
2377 '        If 1 < MNum% Then
2378 '            If 0 < MInspGrNum%(MNum%-1) Then
2379 '                M_04# = 1
2380 '            Else
2381 '                M_04# = 0
2382 '            EndIf
2383 '        Else
2384 '            M_04# = 0
2385 '        EndIf
2386 '        '
2387 '        '�^�X�N�����J�n
2388 '        M_00# = 1                                               'TASK�����J�n
2389 '        '�^�X�N�����J�n�m�F
2390 '        M_Timer(1) = 0
2391 '        MExitFlg = 0
2392 '        While( MExitFlg = 0 )
2393 '            '�����J�n�����m�F
2394 '            If M_00# = 0 And M_10# = 8 Then
2395 '                MExitFlg = 1
2396 '            EndIf
2397 '            'timeout�`�F�b�N
2398 '            If 2000 < M_Timer(1) Then
2399 '                If MNgContinue% = 1 Then                        'NG���s?
2400 '                    MInspErrNumSub = 36                         '�G���[�ԍ��ݒ�36
2401 '                Else
2402 '                    MInspErrNum = 36                            '�G���[�ԍ��ݒ�36
2403 '                EndIf
2404 '                MExitFlg = 1
2405 '            EndIf
2406 '        WEnd
2407 '        '
2408 '        '�����ʒu�ֈړ��E�ړ������҂�
2409 '        If 0 = MInspErrNum Then
2410 '            If MEndFlg% = 0 Then
2411 '                Mvs PInspPos( MNum% )                           '�ړ�
2412 '            EndIf
2413 '        EndIf
2414 '        '
2415 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����I���҂��@INSPTAST1
2416 '        If 0 = MInspErrNum Then
2417 '            M_Timer(1) = 0
2418 '            MExitFlg = 0
2419 '            While( MExitFlg = 0 )
2420 '                '���������҂��i����I���j
2421 '                If M_10# = 1 Then
2422 '                    MExitFlg = 1
2423 '                EndIf
2424 '                '���������҂��i�ُ�I���j
2425 '                If M_10# = 0 Then
2426 '                    If MNgContinue% = 1 Then                    'NG���s?
2427 '                        MInspErrNumSub = M_12#                  '�G���[�ԍ��ݒ�@M12
2428 '                    Else
2429 '                        MInspErrNum = M_12#                     '�G���[�ԍ��ݒ�@M12
2430 '                    EndIf
2431 '                    MExitFlg = 1
2432 '                EndIf
2433 '                'timeout�`�F�b�N
2434 '                If 5000 < M_Timer(1) Then
2435 '                    If MNgContinue% = 1 Then                    'NG���s?
2436 '                        MInspErrNumSub = 31                     '�G���[�ԍ��ݒ�31
2437 '                    Else
2438 '                        MInspErrNum = 31                        '�G���[�ԍ��ݒ�31
2439 '                    EndIf
2440 '                    MExitFlg = 1
2441 '                EndIf
2442 '            WEnd
2443 '        EndIf
2444 '        '
2445 '        '�������ʊm�F
2446 '        If 0 = MInspErrNum Then
2447 '            If 1 < MNum% Then
2448 '                If 0 < MInspGrNum%(MNum%-1) Then                '��������?
2449 '                    If M_11# = 2 Then                           '����NG?
2450 '                        If MNgContinue% = 1 Then                'NG���s?
2451 '                            If MInspNGStepNum = 0 Then          'NG������?
2452 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2453 '                            EndIf
2454 '                            MInspErrNumSub = 32                 '�G���[�ԍ��ݒ� 32:����NG
2455 '                        Else
2456 ''                            MInspNGStepNum = MNum% - 1          '�������sNGStep�ԍ��ݒ�
2457 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2458 '                            MInspErrNum = 32                    '�G���[�ԍ��ݒ� 32:����NG
2459 '                        EndIf
2460 '                   EndIf
2461 '                EndIf
2462 '            EndIf
2463 '        EndIf
2464 '        '
2465 '        '�G���[�Ȃ猟�����f�I������̂�Loop���甲���邽�ߏI���t���O�Z�b�g
2466 '        If 0 <> MInspErrNum Then
2467 '            MEndFlg% = 1
2468 '        EndIf
2469 '        '
2470 '        '�������s�A�捞�����҂�
2471 '        If 0 = MInspErrNum Then
2472 '            If MEndFlg% = 0 Then
2473 '                If 0 < MInspGrNum%(MNum%) Then                  '��������?
2474 '                    M_Out(MOUT_IS_Insp%) = 1                    '�������s�v��on
2475 '                    '�捞�����m�F
2476 '                    M_Timer(1) = 0
2477 '                    MExitFlg = 0
2478 '                    While( MExitFlg = 0 )
2479 '                        '���������҂�
2480 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2481 '                            MExitFlg = 1
2482 '                        EndIf
2483 '                        'timeout�`�F�b�N
2484 '                        If 2000 < M_Timer(1) Then
2485 '                            If MNgContinue% = 1 Then            'NG���s?
2486 '                                MInspErrNumSub = 33             '�G���[�ԍ��ݒ�33
2487 '                            Else
2488 '                                MInspErrNum = 33                '�G���[�ԍ��ݒ�33
2489 '                            EndIf
2490 '                            MExitFlg = 1
2491 '                        EndIf
2492 '                    WEnd
2493 '                EndIf
2494 '                '
2495 '            EndIf
2496 '        EndIf
2497 '        MNum% = MNum% + 1
2498 '    WEnd
2499 '    '
2500 '    '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2501 '    If 0 < MZAxis% Then
2502 '        PCurrentPos = P_Curr                                    '���݈ʒu�擾
2503 '        PCurrentPos.Z = MZAxis%                                 'Z����ݒ�
2504 '        Mvs PCurrentPos                                         '���݈ʒu���ֈړ�
2505 '    EndIf
2506 '    '
2507 '    'NG���s������
2508 '    If MNgContinue% = 1 Then                                    'NG���s?
2509 '        MInspErrNum = MInspErrNumSub                            '�G���[�ԍ��ݒ�
2510 '    EndIf
2511 '    '
2512 '    '�߂�l�ݒ�
2513 '    If MInspErrNum = 0 Then
2514 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2515 '    Else
2516 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2517 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2518 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2519 '    EndIf
2520 '    '
2521 '*ISInspection_End
2522 'FEnd
2523 '
2524 '��InitialZoneB
2525 ''' <summary>
2526 ''' ����~��̕��A����
2527 ''' 1)���ޔ��@Z������Ɉړ�
2528 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
2529 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
2530 ''' 4)�C�j�V�����|�W�V�����ֈړ�
2531 ''' </summary>
2532 ''' <remarks>
2533 ''' Date : 2022/04/08 : N.Watanabe
2534 ''' </remarks>
2535 Function V fnInitialZoneB()
2536     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/04/26 �n��
2537 '
2538 '�p�����[�^
2539     Ovrd 5
2540 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2541 '    Cmp Pos, &B100011
2542 '
2543 '���A����J�n
2544 '
2545 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������
2546 *RecoveryChuckOpen
2547     PActive = P_Curr          '���݈ʒu���擾
2548     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
2549 'PProductOnRoboSet(�˂����{���i�u���ʒu)�́A�`���b�N���
2550     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2551         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2552             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2553                 MRecoveryChuckOpen = 1
2554             EndIf
2555         EndIf
2556     EndIf
2557 'PProductOnRoboGet(�˂����{���i���ʒu)�́A�`���b�N���
2558     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2559         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2560             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2561                 MRecoveryChuckOpen = 1
2562             EndIf
2563         EndIf
2564     EndIf
2565 '
2566     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2567     M_Out(12256) = 0                           '�{�̃`���b�N��OFF
2568     M_Out(12257) = 1                           '�{�̃`���b�N�JON
2569 '
2570     M_20# = 0                                  'KEY���͏�����
2571     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
2572     If MRtn = 1 Then M_Out(12257) = 0          '�{�̃`���b�N�JOFF
2573     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2574 '
2575     fErrorProcess(11,244,284,0)
2576     If M_20# = MNext% Then M_20# = MClear%
2577     If M_20# = MAbout% Then GoTo *RecoveryEnd
2578     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2579     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2580 '
2581     *RecoveryChuckOpenEnd
2582 '
2583 '�w�ʔ��
2584 'PPlateBackSet�`PPlateBackSet_6�̃G���A�ɂ���Ƃ��́A�{�̃`���b�N�J��
2585 '�EPPlateBackSet_6         '�o�H6
2586 '�EPPlateBackSet_5         '�o�H7
2587 '�EPPlateBackSet_4         '�o�H8
2588 '�EPPlateBackSet_3         '�o�H9
2589 '�EPPlateBackSet_2         '�o�H10
2590 '�EPPlateBackSet_1         '�o�H11
2591 '�EPPlateBackSet           '�w�ʔu���ʒu
2592 '��L�V�_�̂w���W�E�x���W�E�y���W��J6�������LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2593     PActive = P_Curr                    '���݈ʒu���擾
2594     JActive = J_Curr                    '���݈ʒu���擾
2595     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2596     If (PActive.X >= -35) And (PActive.X <= -5) Then
2597         If (PActive.Y >= 340) And (PActive.Y <= 510) Then
2598             If (PActive.Z >= 470) And (PActive.Z <= 560) Then
2599                 If (MJ6 >= 160) And (MJ6 <= 200) Then
2600                     M_Out(12256) = 0            '�{�̃`���b�N��OFF
2601                     M_Out(12257) = 1            '�{�̃`���b�N�JON
2602                 Dly 1.0
2603                 EndIf
2604             EndIf
2605         EndIf
2606     EndIf
2607 '
2608 '
2609 '�������@���ځA���ޔ����o���Ȃ����̑Ώ�
2610 '
2611     Ovrd 1
2612 'PProductOnRoboSet(Get)�`PProductOnRoboSet(Get)_2�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_2��
2613 '�EPProductOnRoboSet
2614 '�EPProductOnRoboSet_1
2615 '�EPProductOnRoboSet_2
2616 '�EPProductOnRoboGet
2617 '�EPProductOnRoboGet_1
2618 '�EPProductOnRoboGet_2
2619 '��L�U�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2620     PActive = P_Curr                    '���݈ʒu���擾
2621     JActive = J_Curr                    '���݈ʒu���擾
2622     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2623     If (PActive.X >= -35) And (PActive.X <= 0) Then
2624         If (PActive.Y >= 350) And (PActive.Y <= 420) Then
2625             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
2626                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2627                     Mvs PProductOnRoboSet_1
2628                     Dly 1.0
2629                     Mvs PProductOnRoboSet_2
2630                     Dly 1.0
2631                     Mov PProductOnRoboSet_3
2632                     Dly 1.0
2633                 EndIf
2634             EndIf
2635         EndIf
2636     EndIf
2637 '
2638 'PProductOnRoboSet(Get)_2�`PProductOnRoboSet(Get)_3�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_3��
2639 '�EPProductOnRoboSet_2
2640 '�EPProductOnRoboSet_3
2641 '�EPProductOnRoboGet_2
2642 '�EPProductOnRoboGet_3
2643 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2644     PActive = P_Curr                    '���݈ʒu���擾
2645     JActive = J_Curr                    '���݈ʒu���擾
2646     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2647     If (PActive.X >= -35) And (PActive.X <= 0) Then
2648         If (PActive.Y >= 280) And (PActive.Y <= 390) Then
2649             If (PActive.Z >= 410) And (PActive.Z <= 570) Then
2650                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2651                     Mvs PProductOnRoboSet_3
2652                     Dly 1.0
2653                 EndIf
2654             EndIf
2655         EndIf
2656     EndIf
2657 '
2658     Ovrd 5
2659 '
2660 '���ޔ�
2661     PActive = P_Curr
2662     Pmove = PActive
2663     Pmove.Z = 640           '���ޔ�����ꗥ�̍���
2664     If PActive.X > 550 Then
2665         Pmove.Z =550        '�p���b�g��ɘr��L�΂��Ă���Ƃ���640�܂ŏグ���Ȃ��ׁA��O���u
2666     EndIf
2667     If PActive.Z < Pmove.Z Then
2668         Mvs Pmove
2669     EndIf
2670     Dly 1.0
2671 'J1���ȊO��ޔ��|�W�V�����ֈړ�
2672     JActive = J_Curr
2673     Jmove = JTaihi
2674     Jmove.J1 = JActive.J1        'J1���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
2675     Jmove.J6 = JActive.J6        'J6���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
2676     Mov Jmove
2677     Dly 1.0
2678 'J1���݂̂�ޔ��|�W�V�����ֈړ�
2679     Mov JTaihi
2680     Dly 1.0
2681 '�C�j�V�����|�W�V�����ֈړ�
2682     Mov PInitialPosition
2683     Cmp Off
2684     Ovrd 100
2685 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n
2686     If M_In(11856) = 0 Then                 ' ��~���̂�
2687         fnAutoScreenComment(501)            ' ��ԕ\��[�l�W���ߋ@�����^�]�J�n��] 2022/04/25 �n��
2688         M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
2689         MRet = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
2690         If MRet = 0 Then
2691         Else
2692             M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
2693         EndIf
2694     EndIf
2695     M_Out(12262) = 0            '�ʒu���ߏoOFF
2696     M_Out(12263) = 1            '�ʒu���ߖ�ON
2697     fErrorProcess(11,253,281,0)
2698 *RecoveryEnd
2699     Exit Function
2700 FEnd
2701 '
2702 '
2703 '��fnAutoScreenComment
2704 ''' <summary>
2705 ''' ���C����ʂ̓���󋵕\��
2706 ''' �R�����gD1005�̐ݒ�
2707 ''' </summary>
2708 '''<param name="McommentD1005%">�R�����gID</param>
2709 ''' <remarks>
2710 ''' Date   : 2021/07/07 : M.Hayakawa
2711 ''' </remarks>
2712 Function fnAutoScreenComment(ByVal McommentD1005%)
2713     M_Out16(12576) = McommentD1005%
2714 FEnd
2715 '
2716 '��fnRoboPosChk
2717 ''' <summary>
2718 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
2719 ''' </summary>
2720 '''<param name="MINNumber%">���͔ԍ�</param>
2721 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2722 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2723 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
2724 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
2725 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2726 ''' <remarks>
2727 ''' Date   : 2021/07/07 : M.Hayakawa
2728 ''' </remarks>
2729 Function M% fnRoboPosChk
2730     fnRoboPosChk = 0
2731     MRet = fnStepRead()
2732     '�����ʒu�łȂ��Ɣ��f�����ꍇ
2733     '�E�B���h��ʐ؊���
2734     If MRBTOpeGroupNo > 5 Then
2735         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2736         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
2737         Dly 0.2
2738         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
2739         Dly 1.5
2740         '
2741         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2742         '
2743         MLoopFlg% = 1
2744         While MLoopFlg% = 1
2745             '
2746             '
2747             MKeyNumber% = fnKEY_WAIT()
2748             Select MKeyNumber%
2749                 Case Is = MAbout%       '��~
2750                     M_20# = MAbout%
2751                     MLoopFlg% = -1
2752                     Break
2753                 Case Is = MNext%        '����
2754                     'MLoopFlg% = -1
2755                     Break
2756                 Case Is = MContinue%    '�p��
2757                     M_20# = MContinue%
2758                     MLoopFlg% = -1
2759                     Break
2760                 Default
2761                     Break
2762             End Select
2763         WEnd
2764     EndIf
2765     '
2766     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
2767         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2768         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
2769         Select MRBTOpeGroupNo
2770             Case Is = 5                          '�������Ȃ�
2771                 Break
2772             Case Is = 10                         '�����ʒu�֖߂�
2773                 'Mov PTEST001
2774                 Break
2775             Case Is = 15                         '�����ʒu�֖߂�
2776                 'Mov PTEST002
2777                 Dly 0.5
2778                 'Mov PTEST001
2779                 Dly 0.5
2780                 Break
2781             Default
2782                 Break
2783         End Select
2784         '
2785         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
2786         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
2787         MRBTOpeGroupNo = 5
2788         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
2789         Dly 1.0
2790         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
2791         fnRoboPosChk = 1                        '�����ʒu������s
2792         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
2793     EndIf
2794     Exit Function
2795 FEnd
2796 '
2797 '��frInCheck
2798 ''' <summary>
2799 ''' �Z���T�[IN�`�F�b�N
2800 ''' </summary>
2801 '''<param name="MINNumber%">���͔ԍ�</param>
2802 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
2803 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
2804 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
2805 ''' <remarks>
2806 ''' Date   : 2021/07/07 : M.Hayakawa
2807 ''' </remarks>
2808 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
2809     M_Timer(4) = 0
2810     MloopFlg = 0
2811     While MloopFlg = 0
2812         MCrtTime& = M_Timer(4)
2813         If M_In(MINNumber%) = MCMPFLG% Then
2814             MloopFlg = 1
2815             frInCheck = 1
2816         ElseIf MCrtTime& > MTimeCnt& Then
2817             MloopFlg = 1
2818             frInCheck = 0
2819         EndIf
2820     WEnd
2821 FEnd
2822 '-----------------------------------------------
2823 '
2824 '�˂����ߋ@�ʐM�m�F
2825 '
2826 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2827 'fScrewTcomChk = 0�@�F����I��
2828 '          �@ �@ -1 �F�ُ�I��
2829 '-----------------------------------------------
2830 Function M% fScrewTcomChk
2831 *ReCheckScewTcomChk
2832     fScrewTcomChk = 0
2833     '�ʐM�m�F���M
2834     M_Out(MOUT_ScwT_ComChk%) = MOn%
2835     '�ʐM�m�F��M�ҋ@
2836 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
2837     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
2838     '�ʐM�m�F���M�I��
2839     M_Out(MOUT_ScwT_ComChk%) = MOff%
2840     If MRtn = 0 Then
2841         fScrewTcomChk = -1
2842     EndIf
2843     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
2844  '
2845 FEnd
2846 '
2847 '
2848 '-----------------------------------------------
2849 '
2850 '�˂����ߊJ�n���M
2851 '
2852 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2853 'fScrewTStart = 0�@�F����I��
2854 '           �@�@-1 �F�ُ�I��
2855 '-----------------------------------------------
2856 Function M% fScrewTStart
2857     fScrewTStart = 0
2858     nRet% = 0
2859     '�˂����ߊJ�n�ҋ@����M
2860 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
2861     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
2862     If MRtn = 0 Then nRet% = -1
2863     If MRtn = 0 Then GoTo *ScrewStartERROR      '�J�n�ł��Ȃ������ꍇ�W�����v
2864     Dly 0.1
2865     '�˂����ߊJ�n��M�𑗐M
2866     M_Out(MOUT_ScwT_ST%) = MOn%
2867     Dly 0.5
2868     'Wait M_In(MTEST_KEY%) = MOn%
2869     '�˂����ߊJ�n���M�I��
2870     M_Out(MOUT_ScwT_ST%) = MOff%
2871     '
2872 *ScrewStartERROR
2873     fScrewTStart = nRet%
2874 FEnd
2875 '
2876 '
2877 '
2878 '-----------------------------------------------
2879 '
2880 '�˂����ߊ�����M
2881 '
2882 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2883 'fScewTFinish = 0�@�F����I��
2884 '          �@ �@-1 �F�ُ�I��
2885 '-----------------------------------------------
2886 Function M% fScewTFinish
2887 *ReCheckScewTFinish
2888     fScewTFinish = 0
2889     '�˂����ߊ����ҋ@����M
2890 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
2891     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
2892     If MRtn = 0 Then
2893         fScewTFinish = -1
2894     EndIf
2895     If MRtn = 2 Then GoTo *ReCheckScewTFinish
2896     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
2897     Dly 0.1
2898     '�˂����ߊ�����M�𑗐M
2899     M_Out(MOUT_ScwT_FinOK%) = MOn%
2900     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
2901     '�˂����ߊJ�n���M�I��
2902     M_Out(MOUT_ScwT_FinOK%) = MOff%
2903     'Wait M_In(MTEST_KEY%) = MOn%
2904     '
2905 *ScewTFinish_ErrEnd
2906 FEnd
2907 '
2908 '
2909 '-----------------------------------------------
2910 '
2911 '����xx��~��M
2912 '
2913 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
2914 'fScewTCaseStop = 0�@�F����I��
2915 '          �@   �@-1 �F�ُ�I��
2916 '-----------------------------------------------
2917 Function M% fScewTCaseStop(ByVal MCase%())
2918 *ReCheckScewTCaseStop
2919     fScewTCaseStop = 0
2920     '����xx��~����M
2921     Wait M_In(MCase%(1)) = MOn%
2922     MRtn = fTimeOutJudge(MCase%(1),MOn%)
2923     If MRtn = 0 Then
2924         fScewTCaseStop = -1
2925     EndIf
2926     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
2927     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
2928     Dly 0.1
2929     '����xx��~��M�𑗐M
2930     M_Out(MCase%(2)) = MOn%
2931     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
2932     '�˂����ߊJ�n���M�I��
2933     M_Out(MCase%(2)) = MOff%
2934 *ScewTCaseStop_ErrEnd
2935     '
2936 FEnd
2937 '
2938 '��fScrewTighenRoboCheck
2939 '<summary>
2940 '�˂����{�Ď�
2941 '</summary>
2942 '<param name = "MStopNum%"> ��~�ԍ�</param>
2943 '<returns>���� 0:�˂����{�ُ�I�� 1:OK </returns>
2944 '<make>
2945 '2021/12/2 �����V��
2946 '</make>
2947 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
2948     fnAutoScreenComment(503)    '��ԕ\��[�˂����{����I���҂�] 2022/04/26 �n��
2949     fScrewTighenRoboCheck = 1
2950     MScrewTighenRoboFlg% = 1    '�t���O�̏�����
2951     MCheck% = 0
2952     While MScrewTighenRoboFlg% = 1
2953         MCheck% = M_In16(11904)
2954         If M_In(MStopNum%) = 1 Then '��~�ʒu�܂ŗ�����
2955             MScrewTighenRoboFlg% = 0 '�֐��𔲂���
2956             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
2957         EndIf
2958         If MCheck% <> 0 Then
2959             fScrewTighenRoboError(MCheck%)
2960             Select M_20#
2961                 Case MAbout%            '��~�������ꂽ�ꍇ
2962                     M_Out(12869) = 1 Dly 1.0
2963                     MScrewTighenRoboFlg% = 0
2964                     fScrewTighenRoboCheck = 0   '�ُ�I��
2965                     Break
2966                 Case MNgProcess%        'NG�������ꂽ�ꍇ
2967                     M_Out(12873) = 1 Dly 1.0
2968                     MScrewTighenRoboFlg% = 0
2969                     fScrewTighenRoboCheck = 0   '�ُ�I��
2970                     Break
2971                 Case MContinue%             '���g���C�������ꂽ�ꍇ
2972                     M_20# = MClear%         'M_20#������
2973                     M_Out(12871) = 1 Dly 1.0
2974                     Break
2975                 Case MNext%                 '���ւ������ꂽ�ꍇ
2976                     M_20# = MClear%         'M_20#������
2977                     M_Out(12874) = 1 Dly 1.0
2978                     Break
2979             End Select
2980             Dly 0.5
2981         EndIf
2982     WEnd
2983 FEnd
2984 '
2985 '��fScrewTighenRoboError
2986 '<summary>
2987 '�˂����{�G���[����
2988 '</summary>
2989 '<param name = "ErrorCode%"> �G���[�ԍ�</param>
2990 '<make>
2991 '2021/12/2 �����V��
2992 '</make>
2993 Function fScrewTighenRoboError(ByVal MErrorCode%)
2994     MErrorScreenCode% = 0
2995     MErrorScreenCode% = MErrorCode% + 300
2996     fErrorProcess(11,MErrorScreenCode%,0,0)
2997 FEnd
2998 '
2999 '��fErrorProcess
3000 '<summary>
3001 '�G���[����
3002 '</summary>
3003 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
3004 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
3005 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
3006 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
3007 '<make>
3008 '2021/11/5 �����V��
3009 '</make>
3010 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3011     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
3012     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
3013     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
3014     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
3015 *RETRY_ERR_PROCESS
3016      M_20# = MClear%     '������
3017 '        '�G���[�����L�q
3018         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3019 '        'GOT KEY���͑҂�
3020         MKeyNumber = fnKEY_WAIT()
3021 '        '
3022         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
3023             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3024             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3025             Break
3026          '
3027         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
3028             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3029             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3030         '
3031         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
3032             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3033             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3034          '
3035         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
3036             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3037             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3038             Break
3039         '
3040         EndIf
3041         '
3042         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3043 FEnd
3044 '
3045 '��fnTorqueCheck
3046 ''' <summary>
3047 ''' �g���N�`�F�b�N����p�̃��C��
3048 ''' </summary>
3049 ''' <remarks>
3050 ''' Date   : 2021/12/21 : H.AJI
3051 ''' </remarks>'
3052 Function M% fnTorqueCheck
3053     '�g���N�`�F�b�N�����M  �����n��~
3054     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3055     '
3056     fnTorqueCheck = 0
3057     Ovrd 20
3058     Mov PInitialPosition              '�����ʒu�ړ�
3059     Ovrd 100
3060     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3061     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3062     Dly 0.2
3063     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3064     '
3065     'M6340  �g���N�`�F�b�N��M
3066     'Dly 5.0
3067     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3068     Dly 1.0
3069     M_Out(12340) = 0
3070     '
3071     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3072     M_Out(12835) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3073    Wait M_In(11843) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3074     M_Out(12835) = 0                         '�˂����{�g���N�`�F�b�N��ʐؑ֊���
3075     '
3076     '
3077     MLoopFlg = 1
3078     While MLoopFlg = 1
3079         '
3080         Mov PInitialPosition              '�����ʒu�ړ�
3081         '
3082         MKeyNumber = fnKEY_WAIT()
3083         Select MKeyNumber
3084             Case Is = 1           '��~
3085                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3086                 Dly 1.0
3087                 M_Out(12343) = 0
3088                 Ovrd 20
3089                 'Mov PTicketRead_1
3090                 M_Out(12840) = 1          '�g���N�`�F�b�N�I��
3091                 Wait M_In(11859) = 1      '�˂����{����̏I��
3092                 M_Out(12840) = 0          '�g���N�`�F�b�N�I��
3093                 Ovrd 100
3094                 M_20# = 1
3095                 MLoopFlg = -1
3096                 Break
3097             Case Is = 2           '����
3098                 Break
3099             Case Is = 3           '�p��
3100                 Break
3101             Case Is = 4           '�g���N�`�F�b�N�J�n
3102                 M_Out(12342) = 1          '�g���N�`�F�b�N�J�n�v����M M6342
3103                 Dly 1.0
3104                 M_Out(12342) = 0
3105                 If M_In(11862) = 1 Then             '�g���N�`�F�b�J�[�m�F
3106                     fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3107                     MRet = fnScrewMTorque()           '�˂����{�p�g���N�`�F�b�N
3108                 EndIf
3109                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3110                 'MRet = fnMoveTorquePosi()
3111                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3112                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3113                 Break
3114             Default
3115                 Break
3116         End Select
3117     WEnd
3118     '
3119     '�g���N�`�F�b�N����~���M
3120     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3121     '
3122     '���{�b�g�̈ʒu�����ɖ߂�
3123     '
3124     '
3125  FEnd
3126  '
3127 '
3128 '
3129 '---------------------------
3130 '
3131 '    ���C����ʂ̕\���A��\���ݒ�
3132 '         �R�����gD1001, D1002, D1003�̐ݒ�
3133 '           MWindReSet = 0     ��ʔ�\��
3134 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3135 '           MWindErrScr = 10    �G���[��� D1001, D1002
3136 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3137 '
3138 '---------------------------
3139 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3140     fnMainScreenOpen = 0
3141     '
3142    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3143         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3144     EndIf
3145     '
3146     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3147         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3148     EndIf
3149     '
3150     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3151         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3152     EndIf
3153     '
3154     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3155     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3156     Dly 0.5
3157     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3158 FEnd
3159 '
3160 '��Main
3161 ''' <summary>
3162 ''' �g���N�`�F�b�N������
3163 ''' </summary>
3164 ''' <remarks>
3165 ''' Date   : 2021/12/21 : H.AJI
3166 ''' </remarks>'
3167 Function M% fnScrewMTorque
3168     fnScrewMTorque = 0
3169     M_Out(12838) = 1                         '�g���N�`�F�b�N�J�n1
3170     Wait M_In(11857) = 1                     '��M����
3171     M_Out(12838) = 0                         '�g���N�`�F�b�N�J�n1
3172     Dly 2.0
3173 FEnd
3174 '
3175 '
3176 '----------------------------------------------------------------
3177 'fTimeOutJudge
3178 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3179 '����
3180 'Address% = �Ď��A�h���X�ԍ�
3181 'JudgeFlg% = �ΏۃA�h���X�̐���I�����̒l
3182 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3183 '�߂�l = 0 �G���[
3184 '         1 ����I��
3185 '         2 ���g���C
3186 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3187 '�쐬��
3188 '2022/9/20 ����
3189 '----------------------------------------------------------------
3190 '
3191 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3192     fTimeOutJudge = 0
3193     MJudge% = 1
3194     MRtn = 0
3195     M_20# = MClear%
3196     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3197 *TimeOutLoop
3198     If MRtn = 1 Then GoTo *TimeOut
3199         fErrorProcess(11,202,203,0)
3200         If M_20# = MNext% Then GoTo *TimeOutLoop
3201         If M_20# = MContinue% Then MJudge% = 2
3202         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3203 *TimeOut
3204     fTimeOutJudge = MJudge%
3205 '
3206 *JUDGE_ERROR_END
3207 FEnd
3208 '��Main
3209 ''' <summary>
3210 ''' �g������p�̃��C��
3211 ''' </summary>
3212 ''' <remarks>
3213 ''' Date   : 2021/07/07 : M.Hayakawa
3214 ''' </remarks>'
3215 Function Main
3216     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3217     '
3218     If M_Svo=0 Then
3219         Servo On
3220     EndIf
3221     Wait M_Svo=1
3222 '�g���X�^�[�g���t�����v���p���XON
3223     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3224 '�p�g���C�g����
3225     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3226     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3227     '
3228     M_20# = 0                                   'KEY���͏�����
3229     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
3230     MRet% = 0
3231 '�����ʒu�̊m�F�ƈړ�
3232 '
3233 '���A����@���s�E�����s����      2022/04/08 �n�� �쐬
3234     PActive = P_Curr                    '���݈ʒu���擾
3235     MRecoveryPass% = 0
3236     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3237         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3238             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3239                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3240             EndIf
3241         EndIf
3242     EndIf
3243     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3244         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3245             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3246                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3247             EndIf
3248         EndIf
3249     EndIf
3250     If MRecoveryPass% = 0 Then
3251        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3252     EndIf
3253 '
3254     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3255         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3256 '�g���N�`�F�b�N
3257         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3258             MRet% = fnTorqueCheck()
3259             Break
3260         Else
3261 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
3262 '                MRtn = InspInit()               '�摜��������������
3263 '            EndIf
3264 '
3265             M_20# = MClear%             '������
3266 '�g���J�n
3267             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3268                 fnAssyStart()
3269             Else
3270                 M_20# = MPass%
3271             EndIf
3272 '�g���I�����t����
3273             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3274             Wait M_In(11572) = 1            '���t�擾����
3275             Dly 0.1
3276             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3277 '���t�^�[���j�b�g�ւ�OUT
3278             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3279             fnAutoScreenComment(89)         'AUTO��� �g����������
3280             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3281 'OK/NG�t���O�o��
3282             If M_20# <= 0 Then
3283                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3284             ElseIf M_20# = MPass% Then
3285                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3286             EndIf
3287 'PIAS�ɑg������������
3288             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3289                 If M_20# = MPass% Then
3290                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3291                 Else
3292                     'KEY���͂�NG�̏ꍇ
3293                     If M_20# = MNgProcess% Then
3294                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3295                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3296                         MRet% = fnPiasWrite(MNG%)
3297                        nAssyNgQty = nAssyNgQty + 1
3298                     EndIf
3299                     '
3300                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3301                     If M_20# = MAssyOK% Then
3302                             '-----------------------
3303                             'D732 -> D2600 �R�s�[�v��
3304                             M_Out(12566) = 1
3305 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3306                             M_Out(12566) = 0
3307                             '
3308                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3309                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3310                             '��ԍ��ƍ�(PP�͖��g�p�j
3311 '                            MRet% = fnPCBNumberCheck()
3312                         Else
3313                             MRet% = 1
3314                         EndIf
3315                         '
3316                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3317                             If M_20# <> MAbout% Then
3318                                 '�H������OK��������
3319                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3320                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3321                                 MRet% = fnPiasWrite(MOK%)
3322                                 nAssyOkQty = 0
3323                                 nAssyOkQty = nAssyOkQty + 1
3324                             Else
3325                                 nAssyOkQty = nAssyOkQty + 1
3326                             EndIf
3327                         EndIf
3328                     EndIf
3329 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3330 '                    MRet% = fnPiasWrite(MOK%)
3331                 EndIf
3332             Else
3333                 nAssyOkQty = nAssyOkQty + 1
3334             EndIf
3335             '
3336             '�g���I�����t��������
3337             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3338             '�������A�g��OK���A�g��NG��������
3339 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3340             '
3341 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3342 '                '�摜�����I������
3343 '                MRtn = InspQuit()
3344 '            EndIf
3345         EndIf
3346         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3347     EndIf
3348 '�p�g���C�g����
3349     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3350     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3351 'GOT�\��
3352     fnAutoScreenComment(93)  'AUTO��� �H������
3353 FEnd
3354 End
3355 '
3356 '���܂��Ȃ��R�����g
3357 '��΍폜�����
3358 '
3359 '
3360 '
3361 '
3362 '
3363 '
3364 '
JActive=(93.470,-14.830,113.090,0.170,81.440,182.960,0.000,0.000)
Jmove=(93.470,-46.870,111.640,0.000,80.580,182.960,0.000,0.000)
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
