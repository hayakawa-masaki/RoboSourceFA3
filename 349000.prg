1 ' ===================================
2 '
3 '  2100301001 STEP5 Assy1�v���O����
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
171 MOUT_OKNG% = 12226                     'PLC OUT ��OK=1, NG=0 �o��
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
210 Def Inte nInputQty
211 Def Inte nAssyOkQty
212 Def Inte nAssyNgQty
213 Def Inte MScrewNo
214 Def Inte MReTry
215 '===== <IO�ϐ���`> =====
216 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
217 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
218 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
219 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
220 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
221 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
222 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
223 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
224 '
225 Def Inte Y68_VV1            ' �A�[����[�@�l�W�z���o���u
226 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
227 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
228 '
229 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
230 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
231 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
232 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
233 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
234 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
235 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
236 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
237 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
238 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
239 '
240 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
241 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
242 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
243 '
244 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
245 '
246 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
247 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
248 '
249 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
250 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
251 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
252 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
253 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
254 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
255 '
256 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
257 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
258 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
259 '
260 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
261 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
262 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
263 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
264 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
265 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
266 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
267 Y68_VV1%    =  12248    ' �A�[����[�@�l�W�z���o���u '���l12250����12248�֕ύX(8/5����)
268 Y6B_VB1%    =  12250    '�A�[����[�@�z���j��o���u  '���l12251����12250�֕ύX(8/5����)
269 MOUT_VB1%   =  12250    ' �A�[����[�@�l�W�z���j��o���u  '���l12251����12250�֕ύX(8/5����)
270 '
271 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
272 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
273 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
274 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
275 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
276 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
277 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
278 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
279 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
280 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
281 '
282 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
283 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
284 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
285 '
286 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
287 '
288 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
289 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
290 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
291 '
292 '����
293 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
294 Def Inte MOn                            '�o��=1
295 Def Inte MOff                           '�o��=0
296 '
297 '�˂����ߑ��u_�o�̓A�h���X
298 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
299 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
300 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
301 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
302 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
303 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
304 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
305 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
306 '�˂����ߑ��u_���̓A�h���X
307 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
308 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
309 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
310 Def Inte MIN_ScwT_Case1                 '����1��~����M
311 Def Inte MIN_ScwT_Case2                 '����2��~����M
312 Def Inte MIN_ScwT_Case3                 '����3��~����M
313 Def Inte MIN_ScwT_Case4                 '����4��~����M
314 Def Inte MIN_ScwT_Case5                 '����5��~����M
315 '
316 Dim MScwT_Case1%(2)               '����1��~�ϐ�
317 Dim MScwT_Case2%(2)               '����2��~�ϐ�
318 Dim MScwT_Case3%(2)               '����3��~�ϐ�
319 Dim MScwT_Case4%(2)               '����4��~�ϐ�
320 Dim MScwT_Case5%(2)               '����5��~�ϐ�
321 '
322 '����
323 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
324 MOn% = 1                                 '�o�� = 1
325 MOff% = 0                                '�o�� = 0
326 '
327 '�˂����ߋ@_�A�h���X�ݒ�
328 MOUT_ScwT_ComChk% = 12816               '�ʐM�m�F���M
329 MOUT_ScwT_ST% = 12849                   '�˂����ߊJ�n�𑗐M
330 MOUT_ScwT_ReSTOK% = 12850               '�ĊJ�n��M�𑗐M
331 MOUT_ScwT_FinOK% = 12852                '�˂����ߊ�����M�𑗐M
332 MOUT_ScwT_Case1OK% = 12858              '����1��~��M�𑗐M
333 MOUT_ScwT_Case2OK% = 12859              '����2��~��M�𑗐M
334 MOUT_ScwT_Case3OK% = 12860              '����3��~��M�𑗐M
335 MOUT_ScwT_Case4OK% = 12861              '����4��~��M�𑗐M
336 MOUT_ScwT_Case5OK% = 12862              '����5��~��M�𑗐M
337 '
338 MIN_ScwT_comOK% = 11824                 '�˂����ߑ��u����ԐM
339 MIN_ScwT_STRec% = 11857                 '�˂����ߊJ�n����M
340 MIN_ScwT_ReST% = 11858                  '�ĊJ�n����M
341 MIN_ScwT_Fin% = 11860                   '�˂����ߊ�������M
342 MIN_ScwT_Case1% = 11866                 '����1��~�ҋ@����M
343 MIN_ScwT_Case2% = 11867                 '����2��~�ҋ@����M
344 MIN_ScwT_Case3% = 11868                 '����3��~�ҋ@����M
345 MIN_ScwT_Case4% = 11869                 '����4��~�ҋ@����M
346 MIN_ScwT_Case5% = 11870                 '����5��~�ҋ@����M
347 '
348 MScwT_Case1%(1) = MIN_ScwT_Case1%
349 MScwT_Case1%(2) = MOUT_ScwT_Case1OK%
350 MScwT_Case2%(1) = MIN_ScwT_Case2%
351 MScwT_Case2%(2) = MOUT_ScwT_Case2OK%
352 MScwT_Case3%(1) = MIN_ScwT_Case3%
353 MScwT_Case3%(2) = MOUT_ScwT_Case3OK%
354 MScwT_Case4%(1) = MIN_ScwT_Case4%
355 MScwT_Case4%(2) = MOUT_ScwT_Case4OK%
356 MScwT_Case5%(1) = MIN_ScwT_Case5%
357 MScwT_Case5%(2) = MOUT_ScwT_Case5OK%
358 '
359 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
360 Function M% fnAssyStart
361 ' PIAS�`�P�b�g�Ǎ��ݍH�������m�F
362     M_20# = MClear%                       '������
363 *ASSY_ERROR_END
364 *AssyEnd
365 *fnAssyStart_FEndPosi
366 FEnd
367 '
368 '��fnPiasCheck
369 ''' <summary>
370 ''' PIAS�`�P�b�g�Ǎ���
371 ''' </summary>
372 ''' <returns>   0 : NG
373 '''             1 : OK(�Ǎ��݊���)
374 ''' </returns>
375 ''' <remarks>
376 ''' Date   : 2021/07/07 : M.Hayakawa
377 ''' </remarks>'
378 Function M% fnPiasCheck
379     fnPiasCheck = 0
380     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
381     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
382 '
383 *RETRY_PIAS
384     M_20# = MClear%
385     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
386     '
387     '�yID�`�P�b�g�ǂݍ��݁z
388     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
389     MInspGroup%(1) = 1              '����G�ԍ�
390     MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
391 '
392     '�G���[�̏ꍇ
393     If MRtn <> 1 Then
394         'D720 -> D1300 �R�s�[�v��
395         M_Out(12565) = 1
396         Dly 0.5
397         M_Out(12565) = 0
398         '�G���[�����L�q
399         fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
400         'GOT KEY���͑҂�
401         MKeyNumber = fnKEY_WAIT()
402         '
403         Select MKeyNumber
404             Case MNext%         '���ւ�I�������ꍇ
405                 M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
406                 fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
407                 GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
408                 Break
409             Case MAbout%        '��~��I�������ꍇ
410                 M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
411                 fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
412                 GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
413                 Break
414             Case MNgProcess%    'NG��I�������ꍇ
415                 M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
416                 fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
417                 GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
418                 Break
419             Case MContinue%     '�p����I�������ꍇ
420                 fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
421                 M_20# = MContinue%
422                 GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
423                 Break
424         End Select
425     EndIf
426 '----------D720 -> D1300 �R�s�[�v��----------
427     M_Out(12565) = 1
428     Dly 0.5
429     M_Out(12565) = 0
430 '----------�ʐM�m�F������----------
431     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
432     MRtn = 0                ' ������
433     M_20# = MClear%         ' ������
434     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
435     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
436     If MRtn <> 1 Then
437         If M_20# = MContinue% Then
438             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
439         Else
440             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
441         EndIf
442     EndIf
443 '----------�H�������m�F----------
444     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
445     MRtn = 0                ' ������
446     M_20# = MClear%         ' ������
447     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
448     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
449     If MRtn <> 1 Then
450         If M_20# = MContinue% Then
451             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
452         Else
453             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
454         EndIf
455     EndIf
456     '
457     fnPiasCheck = 1
458     *fnPiasCheck_End
459 FEnd
460 '
461 '��fnPCComuCheck
462 ''' <summary>
463 ''' PC-PLC�ʐM�`�F�b�N
464 ''' </summary>
465 ''' <returns>   0 : NG
466 '''             1 : OK(�Ǎ��݊���)
467 ''' </returns>
468 ''' <remarks>
469 ''' Date   : 2021/07/07 : M.Hayakawa
470 ''' </remarks>'
471 Function M% fnPCComuCheck
472     fnPCComuCheck = 0
473     MJudge% = 0                                  '������
474     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
475     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
476     '
477     For MStaNo = 0 To 5
478         '
479         If M_In(MIN_PIAS_ComOK%) = 1 Then
480             'PC�ʐMOK(M400)
481             MJudge% = MOK%
482             MStaNo = 5
483             Break
484         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
485             'toRBT_�ʐM�m�Ftime out
486             MJudge% = MNG%
487             MCommentD1001 = 15
488             MCommentD1002 = 21
489             MStaNo = 5
490             Break
491         Else
492             'toRBT_�ʐM�m�Ftime out
493             MJudge% = MNG%
494             MCommentD1001 = 14
495             MCommentD1002 = 21
496             Break
497         EndIf
498     Next MStaNo
499     '
500     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
501     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
502     '
503     '�G���[���
504     If MJudge% <> MOK% Then
505         M_20# = MClear%     '������
506         '�G���[�����L�q
507         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
508         'GOT KEY���͑҂�
509         MKeyNumber = fnKEY_WAIT()
510         '
511         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
512             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
513             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
514             Break
515         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
516             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
517             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
518             Break
519         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
520             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
521             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
522             Break
523         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
524             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
525             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
526             Break
527         EndIf
528     Else
529         'OK�̏ꍇ
530         fnPCComuCheck = 1
531     EndIf
532 FEnd
533 '
534 '��fnProcessCheck
535 ''' <summary>
536 ''' �H�������m�F
537 ''' </summary>
538 ''' <returns>    1�F�H������OK     0�F�ُ�I��
539 '''             -1�F�O�H������NG  -2�F���H����������
540 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
541 '''             -5�F���������G���[
542 ''' </returns>
543 ''' <remarks>
544 ''' Date   : 2021/07/07 : M.Hayakawa
545 ''' </remarks>'
546 Function M% fnProcessCheck
547     fnProcessCheck = 0
548     MJudge% = MNG%      '��UNG���������Ƃ���
549 '----------�H�������m�F----------
550     MCommentD1001 = 0   '�R�����g������
551     For MStaNo = 0 To 5
552         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
553         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
554         '
555         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
556             MJudge% = MOK%
557             fnAutoScreenComment(85)     ' AUTO���
558             MStaNo = 5
559             Break
560         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
561             MFlgLoop% = 0
562             MJudge% = MNG%
563             MCommentD1001 = 27
564             MCommentD1002 = 22
565             fnAutoScreenComment(94)     ' AUTO���
566             fnProcessCheck = -2         ' NG��-2��Ԃ�
567             MStaNo = 5
568             Break
569         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
570            MJudge% = MNG%
571             MCommentD1001 = 31
572             MCommentD1002 = 22
573             fnAutoScreenComment(83)     ' AUTO���
574             fnProcessCheck = -3         ' NG��-3��Ԃ�
575             MStaNo = 5
576             Break
577         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
578             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
579             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
580             MJudge% = MNG%
581             MCommentD1001 = 32
582             MCommentD1002 = 22
583             fnAutoScreenComment(84)     ' AUTO���
584             fnProcessCheck = -1         ' NG��-1��Ԃ�
585             Dly 1.0
586             '�H�������m�FOFF
587             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
588             Dly 1.0
589            'MStaNo = 5
590             Break
591         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
592             MFlgLoop% = 0
593             MJudge% = MNG%
594             MCommentD1001 = 29
595             MCommentD1002 = 22
596             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
597             fnProcessCheck = -5         ' NG��-5��Ԃ�
598             MStaNo = 5
599             Break
600         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
601             MJudge% = MNG%
602             If MCommentD1001 = 32 Then
603                 '�������Ȃ�
604             Else
605                 MCommentD1001 = 26
606             EndIf
607             MCommentD1002 = 22
608             fnProcessCheck = -4         ' NG��-4��Ԃ�
609             MStaNo = 5
610             Break
611         Else
612             MJudge% = MNG%
613             MCommentD1001 = 28
614             MCommentD1002 = 22
615         EndIf
616     Next MStaNo
617     '�H�������m�FOFF
618     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
619     '�ʉߗ���NG �H�������̏ꍇ
620     If MJudge% = MPass% Then
621         M_20# = MPass%
622     EndIf
623     '
624     '�G���[���
625     If MJudge% <> MOK% Then
626         M_20# = MClear%     '������
627         '�G���[�����L�q
628         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
629         'GOT KEY���͑҂�
630         MKeyNumber = fnKEY_WAIT()
631         '
632         Select MKeyNumber
633             Case MAbout%        '��~��I�������ꍇ
634                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
635                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
636                 Break
637             Case MNext%         '���ւ�I�������ꍇ
638                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
639                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
640                 Break
641             Case MContinue%     '�p����I�������ꍇ
642                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
643                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
644                 Break
645             Case MNgProcess%    'NG��I�������ꍇ
646                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
647                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
648                 Break
649         End Select
650     Else
651         fnProcessCheck = 1  ' OK��1��Ԃ�
652     EndIf
653 FEnd
654 '
655 '��fnPiasWrite
656 ''' <summary>
657 ''' Pias �g�����ʏ����ݗv��
658 ''' </summary>
659 '''<param name="MFlg%">
660 '''                 MOK%(1) = �H��������OK��������
661 '''                 MNG%(0) = �H��������NG��������
662 '''</param>
663 '''<returns></returns>
664 ''' <remarks>
665 ''' Date   : 2021/07/07 : M.Hayakawa
666 ''' </remarks>'
667 Function M% fnPiasWrite(ByVal MFlg%)
668       fnPiasWrite = 0
669 *RETRY_PIASWRITE
670     '
671     '�g��OK(MOK%)�̏ꍇ�@M306 ON
672    '�g��NG(MNG%)�̏ꍇ�@M307 ON
673     If MFlg% = MOK% Then
674         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
675     Else
676         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
677     EndIf
678     Dly 0.1                  '�O�̂���
679     '
680     'Pias�֏����݊J�n M305 -> ON
681     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
682     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
683     '
684     MJudge% = MNG%
685     '
686     For MStaNo = 0 To 5
687         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
688             MJudge% = MOK%
689             'MRet = fnAutoScreenComment(85)  'AUTO���
690             MStaNo = 5
691             Break
692         '
693         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
694             MJudge% = MNG%
695             'MRet = fnAutoScreenComment(85)  'AUTO���
696            MCommentD1001 = 34
697            MCommentD1002 = 25
698             MStaNo = 5
699             Break
700         '
701         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
702             MJudge% = MNG%
703             'MRet = fnAutoScreenComment(85)  'AUTO���
704            MCommentD1001 = 35
705            MCommentD1002 = 25
706             MStaNo = 5
707             Break
708         '
709         ElseIf M_In(11583) = 1 Then                         '�H����������time out
710             MJudge% = MNG%
711             'MRet = fnAutoScreenComment(85)  'AUTO���
712            MCommentD1001 = 36
713            MCommentD1002 = 25
714             MStaNo = 5
715             Break
716         '
717         Else
718             MJudge% = MNG%
719            MCommentD1001 = 42
720            MCommentD1002 = 25
721         '
722         EndIf
723         '
724     Next MStaNo
725     '
726     'Pias�֏����݊J�n M305 -> OfF
727     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
728     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
729     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
730     '
731     '
732     '�ʉߗ���NG �H�������̏ꍇ
733     If MJudge% = MPass% Then
734         M_20# = MPass%
735     EndIf
736     '
737    M_20# = MClear%     '������
738     '
739     '�G���[���
740     If MJudge% < MOK% Then
741     '
742 '�c���Ă���������ł͎g�p���Ȃ����x��
743 *RETRY_ERR_WRITE
744         M_20# = MClear%     '������
745         '�G���[�����L�q
746         MRet = fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
747         'GOT KEY���͑҂�
748         MKeyNumber = fnKEY_WAIT()
749         '
750         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
751             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
752            MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
753             Break
754         '
755         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
756             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
757             MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
758         '
759         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
760             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
761             MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
762         '
763         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
764             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
765            MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
766             Break
767         '
768         EndIf
769         '
770         If M_20# = MClear% Then *RETRY_ERR_WRITE
771         '
772     EndIf
773     '
774     If M_20# = MContinue% Then *RETRY_PIASWRITE
775     '
776     fnPiasWrite = 1
777     '
778 FEnd
779 '
780 '��fnPCBNumberCheck
781 ''' <summary>
782 ''' Pias ��ԍ��ƍ��v��
783 ''' </summary>
784 '''<param name="%"></param>
785 '''<param name="%"></param>
786 '''<returns></returns>
787 ''' <remarks>
788 ''' Date   : 2021/07/07 : M.Hayakawa
789 ''' </remarks>'
790 Function M% fnPCBNumberCheck
791       fnPCBNumberCheck = 0
792     '
793 *RETRY_PCBCHECK
794     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
795     'Pias�֊�ƍ��J�n M310 -> ON
796     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
797     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
798     '
799     MJudge% = MNG%
800     '
801     For MStaNo = 0 To 5
802         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
803             MJudge% = MOK%
804             fnAutoScreenComment(96)  'AUTO���
805             MStaNo = 5
806             Break
807         '
808         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
809             MJudge% = MNG%
810             fnAutoScreenComment(97)  'AUTO���
811             MCommentD1001 = 37
812             MCommentD1002 = 25
813             MStaNo = 5
814             Break
815         '
816         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
817             MJudge% = MNG%
818             fnAutoScreenComment(98)  'AUTO���
819             MCommentD1001 = 38
820             MCommentD1002 = 25
821             MStaNo = 5
822             Break
823         '
824         ElseIf M_In(11580) = 1 Then                         'time out
825             MJudge% = MNG%
826             fnAutoScreenComment(99)  'AUTO���
827             MCommentD1001 = 39
828             MCommentD1002 = 25
829             MStaNo = 5
830             Break
831         '
832         Else
833             MJudge% = MNG%
834            MCommentD1001 = 41
835            MCommentD1002 = 25
836         '
837         EndIf
838         '
839     Next MStaNo
840     '
841     'Pias�֊�ƍ��J�n M310 -> OfF
842     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
843     '
844     '
845     '�ʉߗ���NG �H�������̏ꍇ
846     If MJudge% = MPass% Then
847         M_20# = MPass%
848     EndIf
849     '
850    M_20# = MClear%     '������
851     '
852     '�G���[���
853     If MJudge% < MOK% Then
854     '
855 '�c���Ă���������ł͎g�p���Ȃ����x��
856 *RETRY_ERR_PCBNUMBER
857         M_20# = MClear%     '������
858         '�G���[�����L�q
859         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
860         'GOT KEY���͑҂�
861         MKeyNumber = fnKEY_WAIT()
862         '
863         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
864             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
865             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
866             Break
867         '
868         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
869             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
870             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
871         '
872         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
873             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
874             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
875         '
876         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
877             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
878             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
879             Break
880         '
881         EndIf
882         '
883         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
884         '
885     EndIf
886     '
887     If M_20# = MContinue% Then *RETRY_PCBCHECK
888 FEnd
889 '
890 '��ScrewTight
891 ''' <summary>
892 ''' �˂����߂��s��(S�^�C�g)
893 ''' </summary>
894 '''<param name="PScrewPos()">
895 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
896 '''             PScrewPos(2)    �F�˂����߉��_
897 '''             PScrewPos(10)   �F�˂����ߏI������
898 '''<param name="MScrewType">�l�W�^�C�v(mm/sec)
899 '''             1:6mm S�^�C�g��l�W
900 '''             2:13mm S�^�C�g��l�W
901 '''             3:6mm S�^�C�g���l�W
902 '''             4:3mm S�^�C�g���l�W
903 '''             5:6mm M�l�W
904 '''</param>
905 '''<param name="MFeedSpd">���葬�x(mm/sec)</param>
906 '''<returns>����
907 '''         0=�ُ�I���A1=����I��
908 '''</returns>
909 ''' <remarks>
910 ''' Date   : 2021/07/07 : M.Hayakawa
911 ''' Update : 2021/09/28 : M.Hayakawa �l�W�^�C�v�A���葬�x�������ɒǉ�
912 ''' </remarks>'
913 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   '�l�W���ߌʐݒ�
914     ScrewTight = 0
915     MOKNGFlg = 0
916     Ovrd 100
917     Fine 0.05 , P
918     Mvs PScrewPosition(1)       ' �p���b�g��˂�����S�@�̈��S����ʒu
919 '    Ovrd MOvrdA%               '10/7���ݒlNull
920     Ovrd 20                     '�O�̂��ߌ���
921     ' �p���b�g��˂����ߊJ�n�ʒu�ֈړ�
922     Mvs PScrewPosition(2)
923     ' ����Ovrd�ݒ�
924 '    Ovrd MOvrdA%
925     Ovrd 100
926     ' Spd�ݒ�
927 '    Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
928     Spd MFeedSpd * (100/M_OPovrd)
929     ' �ݒ�i�ݗ�5.0 �~ ����p�l���̃I�[�o�[���C�h�W�� �~ �v���O�������I�[�o�[���C�h�W��
930     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
931     Select MScrewType%
932         Case 1
933             ' S�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
934             ProgramBankSet(1,1)
935             Break
936         Case 2
937             ' S�^�C�g13mm�F�v���O����2�A�o���N1�ɐݒ�
938             ProgramBankSet(2,1)
939             Break
940         Case 3
941             ' S�^�C�g���F�v���O����3�A�o���N1�ɐݒ�
942             ProgramBankSet(3,1)
943             Break
944         Case 4
945             ' S�^�C�g3mm���F�v���O����4�A�o���N1�ɐݒ�
946             ProgramBankSet(4,1)
947             Break
948         Case 5
949             ' M�l�W�F�v���O����5�A�o���N1�ɐݒ�
950             ProgramBankSet(5,1)
951             Break
952         Default
953             ' �v���O����1�A�o���N�Ȃ��ݒ�
954             ProgramBankSet(0,0)
955             Break
956     End Select
957 '
958 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
959      '�h���C�o�[ON�@CW
960     M_Out(12241)=1
961     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
962     Wait M_In(11584)=1          '����/�G���[���o �b��R�����g 10/6 M.H
963     Dly 0.1
964     Spd M_NSpd
965     Fine 0 , P
966     '
967     If M_In(11256)=1 Then  '�˂��g�[�^���G���[���o��
968         M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
969         Dly 0.1
970        ' �v���O�����E�o���N����
971         ProgramBankSet(0,0)
972         '�p���b�g��˂����ߏI���ʒu���ֈړ�
973         Mvs PScrewPosition(10),-80
974         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
975         M_Out(12249)=1 Dly 0.3
976         MOKNGFlg = -1
977         ScrewTight = 0
978     Else
979          '�h���C�o�[OFF�@CW
980         M_Out(12241)=0
981 '        �G���[���Ȃ��ꍇ�̓l�W���ߏI���ʒu�ő�������
982         Select MScrewType%
983             Case 1
984                 ' S�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
985                 ProgramBankSet(1,3)
986                 Break
987             Case 2
988                 ' S�^�C�g13mm�F�v���O����2�A�o���N3�ɐݒ�
989                 ProgramBankSet(2,3)
990                 Break
991             Case 3
992                 ' S�^�C�g���F�v���O����1�A�o���N3�ɐݒ�
993                 ProgramBankSet(3,3)
994                 Break
995             Case 4
996                 ' S�^�C�g13mm�F�v���O����1�A�o���N3�ɐݒ�
997                 ProgramBankSet(4,3)
998                 Break
999             Case 5
1000                 ' M�l�W�F�v���O����1�A�o���N3�ɐݒ�
1001                 ProgramBankSet(5,3)
1002                 Break
1003             Default
1004                 ' �v���O����1�A�o���N�Ȃ��ݒ�
1005                 ProgramBankSet(0,0)
1006                 Break
1007         End Select
1008          '�h���C�o�[ON�@CW
1009         Mvs PScrewPosition(10)
1010         M_Out(12241)=1
1011         Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
1012 '
1013          '�h���C�o�[OFF�@CW
1014         M_Out(12241)=0
1015        ' �v���O�����E�o���N����
1016         ProgramBankSet(0,0)
1017         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1018         M_Out(12249)=1 Dly 0.3
1019     '     ��PScrewPos(2) �� PScrewPosition(10)�ɕύX 9/16 M.Hayakawa
1020         '�p���b�g��˂����ߏI���ʒu���ֈړ�
1021         Mvs PScrewPosition(10),-80
1022         ScrewTight = 1
1023     EndIf
1024 ' �b��i�b��}�X�N�@9/16 M.Hayakawa)
1025 '    Ovrd 10
1026 '    Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
1027     Ovrd 100
1028 FEnd
1029 '
1030 '��ScrewGet
1031 ''' <summary>
1032 ''' �˂������@����˂��𓾂�
1033 ''' </summary>
1034 '''<param name="%">
1035 '''         PScrewPos(1)    �F�˂�������̂˂����
1036 '''         PScrewPos(2)    �F�˂���������_
1037 '''         PScrewPos(9)    �F�˂���������l�W�̈ʒu
1038 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1039 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
1040 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
1041 '''</param>
1042 '''<param name = FeederReadyNo%> �w��̋����@Ready </param>
1043 '''<param name = FeederScrewSensor%> �w��̌닟���h�~�Z���T�[�w��(0�ŃZ���T�[����)</param>
1044 '''<returns>����
1045 '''         0=�ُ�I���A1=����I���A-1=�˂�����NG�A-2=�˂��닟��NG�A-3=�z���G���[
1046 '''</returns>
1047 ''' <remarks>
1048 ''' Date   : 2021/07/07 : M.Hayakawa
1049 ''' </remarks>
1050 '''<update>
1051 '''Date    : 2021/11/15 : ����
1052 '''</update>
1053 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
1054     ScrewGet = 0
1055     MScrewJudge% = 0
1056     '�˂������평������G���[�`�F�b�N
1057 ' ���b��폜
1058     Mov PScrewPosition(2)   '�˂������@���_�ֈړ�
1059     For MCnt% = 0 To MFinCnt%
1060        '�˂������평������G���[�`�F�b�N
1061         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '�w��˂������@��Ready�ɂȂ��Ă��邩�m�F(5�b��)
1062         If MRtn = 0 Then
1063             'Ovrd 30
1064             M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1065             ScrewGet = -1
1066             MScrewJudge% = 2
1067         EndIf
1068         Ovrd 100
1069         If FeederScrewSensor% <> 0 Then
1070             If M_In(FeederScrewSensor%) = 1 Then  '�닟�������o���ꂽ��
1071                 'Ovrd 30
1072                 M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
1073                 'NG�Ƃ��Ă����̊֐����甲����
1074                 ScrewGet = -2
1075                 MScrewJudge% = 3
1076             EndIf
1077         EndIf
1078         Ovrd 100
1079         Spd M_NSpd
1080         '�˂������J�n
1081         If MScrewJudge% = 0 Then
1082     '        ScrewGet = 0
1083             M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
1084             MScrewCnt% = 0
1085             MFinCnt% = 2
1086             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
1087             Ovrd 5 '2�ɕύX 10/6 M.H '5�ɕύX10/7����
1088             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
1089             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
1090             Mvs PScrewPosition(10), 1.2
1091             M_Out(Y68_VV1)=1 Dly 0.3     ' �˂��z���@ON'Dly 0.3�ǉ�(8/27����)
1092             '�r�b�g��]
1093             M_Out(Y60_Driver)=1
1094             Dly 0.2
1095             '
1096             Ovrd 5 '2�ɕύX 10/6 M.H '5�ɕύX10/7����
1097             JOvrd M_NJovrd
1098             Spd M_NSpd
1099             '�l�W�z���m�F�ʒu�ړ�
1100             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
1101             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
1102             '�r�b�g��]��~
1103             M_Out(Y60_Driver)=0
1104             '
1105             '1�b�ԃl�W�z���m�F
1106             MRtn = frInCheck(11268, 1, MSETTIMEOUT01&)
1107             'MRtn = 0'�����G���[
1108             '�z���G���[�̏ꍇ
1109             '�l�W���˂����Y�ɖ߂�
1110             If MRtn = 0 Then
1111                 Ovrd 5      '2����5�ɕύX
1112                 '�r�b�g��]��~
1113                 M_Out(Y60_Driver)=0
1114                 '�l�W�����@���
1115                 Mvs PScrewPosition(1)
1116                 '�X�ɏ��
1117                 Mov PScrewPosition(1), -140
1118                 '�l�W�̂Ĉʒu
1119                 Mov PScrewPosition(9)
1120                 '�z��OFF
1121                 M_Out(12249)=1 Dly 0.3 '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1122                 Dly 0.2
1123                 '�j��ON
1124                 M_Out(Y6B_VB1)=1 '�^��j��ON
1125                 '�r�b�g��]
1126                 M_Out(Y61_Driver)=1
1127                 Dly 0.5
1128                 '                '
1129                 Ovrd 100
1130                 JOvrd M_NJovrd
1131                 Spd M_NSpd
1132                 '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1133                 Mov PScrewPosition(9), 10
1134                 Mov PScrewPosition(9)
1135                 Dly 0.1
1136                 Mov PScrewPosition(9), 10
1137                 Mov PScrewPosition(9)
1138                 '
1139                 '�l�W�����҂�
1140                 Wait M_In(11268) = 0
1141                 '�r�b�g��]��~
1142                 M_Out(Y61_Driver)=0
1143                 Dly 0.1
1144                 '�j��OFF
1145                 M_Out(Y6B_VB1)=0 '�^��j��OFF
1146                 '�˂��������Ƃ��āA�ړ��X�ɏ��
1147                 Mov PScrewPosition(1), -140
1148                 Ovrd 100
1149                 Spd M_NSpd
1150                 '�l�W�����@���
1151                 Mvs PScrewPosition(1)
1152 '                '
1153                 ScrewGet = -3
1154                 Break
1155 '                '
1156             Else
1157                 MCnt% = MFinCnt%
1158                 ScrewGet = 0
1159             EndIf
1160         Else
1161             MCnt% =MFinCnt%
1162         EndIf
1163     Next  MCnt%
1164         '
1165     If MScrewJudge% = 0 Then
1166         Ovrd 100
1167         Spd M_NSpd
1168         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
1169         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
1170         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
1171         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
1172         Mov PScrewPosition(2)
1173         '������x�z���m�F
1174         MRtn = frInCheck(11268, 1, MSETTIMEOUT01&)
1175         If MRtn = 0 Then      '�z���G���[�̏ꍇ
1176             MScrewJudge% = 4
1177             ScrewGet = -3
1178         ElseIf MRtn = 1 Then      '�z��OK�̏ꍇ
1179             MScrewJudge% = 1
1180             ScrewGet = 1
1181         EndIf
1182         Break
1183     EndIf
1184     If MScrewJudge% <> 1 Then                 '�G���[�I����
1185     Select MScrewJudge%
1186         Case 0
1187             fErrorProcess(11,0,0,0) '�ُ�I��
1188             Break
1189         Case 2
1190             fErrorProcess(11,0,0,0) '����NG
1191             Break
1192         Case 3
1193             fErrorProcess(11,0,0,0) '�닟��
1194             Break
1195         Case 4
1196             fErrorProcess(11,94,95,0) '�z��NG
1197             Break
1198     End Select
1199     '
1200     Select M_20#
1201         Case MAbout%          '��~�������ꂽ�ꍇ
1202 '            Mov PScrewPosition(2)                  '�����ʒu�ɖ߂��Ē�~����'�o�H�ɖ�肠��R�����g�A�E�g11/24����
1203 '            Mov PInitialPosition
1204             Break
1205         Case MContinue%       '���g���C��������Ă����ꍇ(�֐��𔲂�����ŏ���)
1206             Break
1207         Case MNext%           '�p���������ꂽ�ꍇ
1208             M_20# = MClear%     '������
1209             Break
1210         Case MNgProcess%      'NG�������ꂽ�ꍇ
1211 '            Mov PScrewPosition(2)   'PIAS��NG�������݂��s��,�����ʒu�ɖ߂��čs���I��'�o�H�ɖ�肠��R�����g�A�E�g11/24����
1212 '            Mov PInitialPosition
1213             Break
1214         End Select
1215     EndIf
1216 FEnd
1217 '
1218 '��ProgramBankSet
1219 ''' <summary>
1220 ''' �˂����߂��s��(P�^�C�g)
1221 ''' </summary>
1222 '''<param name="MProgramNo">�v���O�����ԍ�</param>
1223 '''<param name="MBankNo">�o���N�ԍ�</param>
1224 '''</returns>
1225 ''' <remarks>
1226 ''' Date   : 2021/10/05 : M.Hayakawa
1227 ''' </remarks>'
1228 Function ProgramBankSet(ByVal MProgramNo%,ByVal MBankNo%)
1229     Select MProgramNo%
1230         Case 1
1231             M_Out(Y65_Driver)=0     ' �v���O�����Z�b�e�B���O�@F1
1232             Dly 0.1
1233             M_Out(Y66_Driver)=0     ' �v���O�����Z�b�e�B���O�@F2
1234             Dly 0.1
1235             M_Out(Y67_Driver)=0     ' �v���O�����Z�b�e�B���O�@F3
1236             Dly 0.1
1237             Break
1238         Case 2
1239             M_Out(Y65_Driver)=1     ' �v���O�����Z�b�e�B���O�@F1
1240             Dly 0.1
1241             M_Out(Y66_Driver)=0     ' �v���O�����Z�b�e�B���O�@F2
1242             Dly 0.1
1243             M_Out(Y67_Driver)=0     ' �v���O�����Z�b�e�B���O�@F3
1244             Dly 0.1
1245             Break
1246         Case 3
1247             M_Out(Y65_Driver)=0     ' �v���O�����Z�b�e�B���O�@F1
1248             Dly 0.1
1249             M_Out(Y66_Driver)=1     ' �v���O�����Z�b�e�B���O�@F2
1250             Dly 0.1
1251             M_Out(Y67_Driver)=0     ' �v���O�����Z�b�e�B���O�@F3
1252             Dly 0.1
1253             Break
1254          Case 4
1255             M_Out(Y65_Driver)=1     ' �v���O�����Z�b�e�B���O�@F1
1256             Dly 0.1
1257             M_Out(Y66_Driver)=1     ' �v���O�����Z�b�e�B���O�@F2
1258             Dly 0.1
1259             M_Out(Y67_Driver)=0     ' �v���O�����Z�b�e�B���O�@F3
1260             Dly 0.1
1261             Break
1262         Case 5
1263             M_Out(Y65_Driver)=0     ' �v���O�����Z�b�e�B���O�@F1
1264             Dly 0.1
1265             M_Out(Y66_Driver)=0     ' �v���O�����Z�b�e�B���O�@F2
1266             Dly 0.1
1267             M_Out(Y67_Driver)=1     ' �v���O�����Z�b�e�B���O�@F3
1268             Dly 0.1
1269             Break
1270         Case 6
1271             M_Out(Y65_Driver)=1     ' �v���O�����Z�b�e�B���O�@F1
1272             Dly 0.1
1273             M_Out(Y66_Driver)=0     ' �v���O�����Z�b�e�B���O�@F2
1274             Dly 0.1
1275             M_Out(Y67_Driver)=1     ' �v���O�����Z�b�e�B���O�@F3
1276             Dly 0.1
1277             Break
1278         Case 7
1279             M_Out(Y65_Driver)=0     ' �v���O�����Z�b�e�B���O�@F1
1280             Dly 0.1
1281             M_Out(Y66_Driver)=1     ' �v���O�����Z�b�e�B���O�@F2
1282             Dly 0.1
1283             M_Out(Y67_Driver)=1     ' �v���O�����Z�b�e�B���O�@F3
1284             Dly 0.1
1285             Break
1286         Case 8
1287             M_Out(Y65_Driver)=1     ' �v���O�����Z�b�e�B���O�@F1
1288             Dly 0.1
1289             M_Out(Y66_Driver)=1     ' �v���O�����Z�b�e�B���O�@F2
1290             Dly 0.1
1291             M_Out(Y67_Driver)=1     ' �v���O�����Z�b�e�B���O�@F3
1292             Dly 0.1
1293             Break
1294         Default
1295             M_Out(Y65_Driver)=0     ' �v���O�����Z�b�e�B���O�@F1
1296             Dly 0.1
1297             M_Out(Y66_Driver)=0     ' �v���O�����Z�b�e�B���O�@F2
1298             Dly 0.1
1299             M_Out(Y67_Driver)=0     ' �v���O�����Z�b�e�B���O�@F3
1300             Dly 0.1
1301             Break
1302     End Select
1303 '
1304     Select MBankNo%
1305         Case 1
1306             M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
1307             Dly 0.1
1308             M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
1309             Dly 0.1
1310             M_Out(Y64_Driver)=0     ' �o���N�Z�b�e�B���O�@C3
1311             Dly 0.1
1312             Break
1313         Case 2
1314             M_Out(Y62_Driver)=0     ' �o���N�Z�b�e�B���O�@C1
1315             Dly 0.1
1316             M_Out(Y63_Driver)=1     ' �o���N�Z�b�e�B���O�@C2
1317             Dly 0.1
1318             M_Out(Y64_Driver)=0     ' �o���N�Z�b�e�B���O�@C3
1319             Dly 0.1
1320             Break
1321         Case 3
1322             M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
1323             Dly 0.1
1324             M_Out(Y63_Driver)=1     ' �o���N�Z�b�e�B���O�@C2
1325             Dly 0.1
1326             M_Out(Y64_Driver)=0     ' �o���N�Z�b�e�B���O�@C3
1327             Dly 0.1
1328             Break
1329         Default
1330             M_Out(Y62_Driver)=0     ' �o���N�Z�b�e�B���O�@C1
1331             Dly 0.1
1332             M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
1333             Dly 0.1
1334             M_Out(Y64_Driver)=0     ' �o���N�Z�b�e�B���O�@C3
1335             Dly 0.1
1336             Break
1337     End Select
1338 FEnd
1339 '
1340 '��fnKEY_WAIT()
1341 ''' <summary>
1342 ''' GOT����̃L�[���͑҂�
1343 ''' </summary>
1344 '''<returns>1�F��~    2�F����
1345 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
1346 '''         5�FNG
1347 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
1348 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
1349 '''</returns>
1350 ''' <remarks>
1351 ''' Date   : 2021/07/07 : M.Hayakawa
1352 ''' </remarks>'
1353 Function M% fnKEY_WAIT()
1354     fnKEY_WAIT = 0
1355     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
1356     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
1357     fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
1358     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1359     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
1360     Dly 0.2
1361     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
1362     MLocalLoopFlg=1
1363     While MLocalLoopFlg=1
1364         If M_In(11345) = 1 Then         '��~   M5345
1365             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
1366             fnKEY_WAIT = 1
1367             MLocalLoopFlg=-1
1368             Break
1369         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
1370             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
1371             fnKEY_WAIT = 2
1372             MLocalLoopFlg=-1
1373             Break
1374         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
1375             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
1376             fnKEY_WAIT = 3
1377             MLocalLoopFlg=-1
1378             Break
1379         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
1380             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
1381             fnKEY_WAIT = 4
1382             MLocalLoopFlg=-1
1383             Break
1384         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
1385             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
1386             fnKEY_WAIT = 5
1387             MLocalLoopFlg=-1
1388             Break
1389             '
1390         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
1391             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
1392             fnKEY_WAIT = MRobotInit1%
1393             MLocalLoopFlg=-1
1394             Break
1395             '
1396         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
1397             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
1398             fnKEY_WAIT = MRobotInit2%
1399             MLocalLoopFlg=-1
1400             Break
1401             '
1402         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
1403             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
1404             fnKEY_WAIT = MRobotInit3%
1405             MLocalLoopFlg=-1
1406             Break
1407             '
1408         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
1409             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
1410             fnKEY_WAIT = MRobotInit4%
1411             MLocalLoopFlg=-1
1412             Break
1413             '
1414         Else
1415         EndIf
1416     WEnd
1417     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
1418     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
1419 FEnd
1420 '
1421 '�� fnAUTO_CTL
1422 ''' <summary>
1423 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
1424 ''' </summary>
1425 ''' <remarks>
1426 ''' Date   : 2021/07/07 : M.Hayakawa
1427 ''' </remarks>
1428 Function M% fnAUTO_CTL
1429     fnAUTO_CTL = 0
1430     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
1431     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
1432     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
1433     '
1434     If M_Svo=0 Then             '�T�[�{ON�m�F
1435         Servo On
1436     EndIf
1437     Wait M_Svo=1
1438 FEnd
1439 '
1440 '�� fnWindScreenOpen
1441 ''' <summary>
1442 ''' �E�B���h��ʂ̕\���A��\���ݒ�
1443 ''' </summary>
1444 '''<param name="%"></param>
1445 '''<param name="%"></param>
1446 '''<param name="%"></param>
1447 '''<param name="%"></param>
1448 ''' <remarks>
1449 ''' �R�����gD1001, D1002, D1003�̐ݒ�
1450 ''' MWindReSet = 0     ��ʔ�\��
1451 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
1452 ''' MWindErrScr = 10    �G���[��� D1001, D1002
1453 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
1454 ''' Date   : 2021/07/07 : M.Hayakawa
1455 ''' </remarks>
1456 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
1457     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
1458         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
1459     EndIf
1460     '
1461     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
1462         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
1463     EndIf
1464     '
1465     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
1466        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
1467     EndIf
1468     '
1469     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
1470     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
1471     Dly 0.5
1472     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
1473 FEnd
1474 '
1475 '��fnCtlValue
1476 ''' <summary>
1477 ''' �������A�g��OK���A�g��NG���@Read/Write
1478 ''' </summary>
1479 ''' <param name="MCtlNo%"></param>
1480 ''' <remarks>
1481 ''' Date   : 2021/07/07 : M.Hayakawa
1482 ''' </remarks>
1483 Function fnCtlValue(ByVal MCtlNo)
1484     Select MCtlNo
1485         Case 1        '�Ǎ���
1486             M_Out(12568) = 1        '
1487             M_30# = M_In16(11600)   '��������M
1488             M_31# = M_In16(11616)   '�g��OK��M
1489             M_32# = M_In16(11632)   '�g��NG��M
1490             M_33# = M_In16(11648)   '�g��NG��M
1491             M_Out(12568) = 0
1492             Break
1493             '
1494         Case 2        '������
1495             M_Out(12569) = 1
1496             Dly 0.3
1497             M_Out16(12592) = M_30#  '���������M
1498             M_Out16(12608) = M_31#  '�g��OK���M
1499             M_Out16(12624) = M_32#  '�g��N���M
1500             M_Out16(12640) = M_33#  '�g��N���M
1501             M_Out(12569) = 0
1502             Break
1503     End Select
1504 FEnd
1505 '
1506 ' ��ISInspection
1507 ''' <summary>
1508 ''' Insight�ɂ��摜�����������s
1509 ''' </summary>
1510 '''<param name="PInspPos()">�����ʒu</param>
1511 '''<param name="MInspGrNum%()">�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j</param>
1512 '''             PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
1513 '''<param name="MInspCnt%">�����ʒu��</param>
1514 '''<param name="MZAxis%">�I������Z���ޔ����W�i-1:�����j</param>
1515 '''             �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
1516 '''<param name="MNgContinue%">=1�Ō����G���[�ENG�������ɑSStep�̌������s��</param>
1517 '''<returns>    ���� 0=�ُ�I���A1=����I��</returns>
1518 '''         MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
1519 '''         MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���"
1520 ''' <remarks>
1521 ''' Date   : 2021/07/07 : M.Hayakawa
1522 ''' </remarks>
1523 Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
1524     '�摜�g�p�m�F 0<- �摜�m�F�����̏ꍇ
1525     If M_In(11369) = 0 Then            'toRBT_�g�p�m�F
1526         ISInspection = 1                                        '����I���߂�l�ݒ�
1527     EndIf
1528 '
1529     Cnt 0                                                       '�ړ�����������(�����l=0)
1530     Fine 0.05,P                                                 '�ʒu���ߊ��������ݒu�@0.05mm
1531     MNum% = 1                                                   '�����ԍ������l�ݒ�
1532     Def Inte MEndFlg                                            '�����I���t���O
1533     MEndFlg% = 0
1534     '
1535     '�G���[�ԍ��N���A
1536     MInspErrNumSub = 0                                          '�������s�G���[�ԍ�sub
1537     MInspErrNum = 0                                             '�������s�G���[�ԍ�
1538     M_Out16(MOUT_InspErrNum) = MInspErrNum
1539     MInspNGStepNum = 0                                          '�������sNGStep�ԍ�
1540     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
1541     '
1542     If M_In(MIN_IS_Ready) = 0 Then                              'Ready off�Ȃ�I��
1543         MInspErrNum = 20                                        '�������s�G���[�ԍ� 20 Insight offline
1544         M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
1545         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
1546         ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
1547 '
1548     EndIf
1549    If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
1550     '
1551     '�����ʒu���m�F
1552     If MInspCnt% < 1 Or 30 < MInspCnt% Then
1553         MInspErrNum = 21                                        '�����f�[�^�Ȃ� 21�@����<1
1554         M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
1555         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
1556         ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
1557 '
1558     EndIf
1559    If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
1560     '
1561     '�ݒ肳�ꂽ�����ʒu�����̌������s
1562     While( MEndFlg% = 0 )
1563         '�����I���m�F�@�����I���t���O�Z�b�g
1564         If (MInspCnt% < MNum% ) Then
1565             MEndFlg% = 1                                        '�����I���t���O�Z�b�g
1566         EndIf
1567         '
1568         '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����J�n�@INSPTAST1
1569         If MEndFlg% = 0 Then
1570             M_01# = MInspGrNum%(MNum%)                          '����G�ԍ����n��
1571         EndIf
1572         M_02# = MEndFlg%                                        '�����I���t���O���n��
1573         M_05# = MNum%                                           '�����ԍ�(������1�`)
1574         '�^�X�N�@����G�ݒ�t���O���n��
1575         If MEndFlg% = 0 Then
1576             If 0 < MInspGrNum%(MNum%) Then
1577                 M_03# = 1
1578             Else
1579                 M_03# = 0
1580             EndIf
1581         Else
1582             M_03# = 0
1583         EndIf
1584         '�^�X�N�@�������ʊm�F�t���O���n��
1585         If 1 < MNum% Then
1586             If 0 < MInspGrNum%(MNum%-1) Then
1587                 M_04# = 1
1588             Else
1589                 M_04# = 0
1590             EndIf
1591         Else
1592             M_04# = 0
1593         EndIf
1594         '
1595         '�^�X�N�����J�n
1596         M_00# = 1                                               'TASK�����J�n
1597         '�^�X�N�����J�n�m�F
1598         M_Timer(1) = 0
1599         MExitFlg = 0
1600         While( MExitFlg = 0 )
1601             '�����J�n�����m�F
1602             If M_00# = 0 And M_10# = 8 Then
1603                 MExitFlg = 1
1604             EndIf
1605             'timeout�`�F�b�N
1606             If 2000 < M_Timer(1) Then
1607                 If MNgContinue% = 1 Then                        'NG���s?
1608                     MInspErrNumSub = 36                         '�G���[�ԍ��ݒ�36
1609                 Else
1610                     MInspErrNum = 36                            '�G���[�ԍ��ݒ�36
1611                 EndIf
1612                 MExitFlg = 1
1613             EndIf
1614         WEnd
1615         '
1616         '�����ʒu�ֈړ��E�ړ������҂�
1617         If 0 = MInspErrNum Then
1618             If MEndFlg% = 0 Then
1619                 Mvs PInspPos( MNum% )                           '�ړ�
1620             EndIf
1621         EndIf
1622         '
1623         '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����I���҂��@INSPTAST1
1624         If 0 = MInspErrNum Then
1625             M_Timer(1) = 0
1626             MExitFlg = 0
1627             While( MExitFlg = 0 )
1628                 '���������҂��i����I���j
1629                 If M_10# = 1 Then
1630                     MExitFlg = 1
1631                 EndIf
1632                 '���������҂��i�ُ�I���j
1633                 If M_10# = 0 Then
1634                     If MNgContinue% = 1 Then                    'NG���s?
1635                         MInspErrNumSub = M_12#                  '�G���[�ԍ��ݒ�@M12
1636                     Else
1637                         MInspErrNum = M_12#                     '�G���[�ԍ��ݒ�@M12
1638                     EndIf
1639                     MExitFlg = 1
1640                 EndIf
1641                 'timeout�`�F�b�N
1642                 If 5000 < M_Timer(1) Then
1643                     If MNgContinue% = 1 Then                    'NG���s?
1644                         MInspErrNumSub = 31                     '�G���[�ԍ��ݒ�31
1645                     Else
1646                         MInspErrNum = 31                        '�G���[�ԍ��ݒ�31
1647                     EndIf
1648                     MExitFlg = 1
1649                 EndIf
1650             WEnd
1651         EndIf
1652         '
1653         '�������ʊm�F
1654         If 0 = MInspErrNum Then
1655             If 1 < MNum% Then
1656                 If 0 < MInspGrNum%(MNum%-1) Then                '��������?
1657                     If M_11# = 2 Then                           '����NG?
1658                         If MNgContinue% = 1 Then                'NG���s?
1659                             If MInspNGStepNum = 0 Then          'NG������?
1660                                 MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
1661                             EndIf
1662                             MInspErrNumSub = 32                 '�G���[�ԍ��ݒ� 32:����NG
1663                         Else
1664 '                            MInspNGStepNum = MNum% - 1          '�������sNGStep�ԍ��ݒ�
1665                             MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
1666                             MInspErrNum = 32                    '�G���[�ԍ��ݒ� 32:����NG
1667                         EndIf
1668                    EndIf
1669                 EndIf
1670             EndIf
1671         EndIf
1672         '
1673         '�G���[�Ȃ猟�����f�I������̂�Loop���甲���邽�ߏI���t���O�Z�b�g
1674         If 0 <> MInspErrNum Then
1675             MEndFlg% = 1
1676         EndIf
1677         '
1678         '�������s�A�捞�����҂�
1679         If 0 = MInspErrNum Then
1680             If MEndFlg% = 0 Then
1681                 If 0 < MInspGrNum%(MNum%) Then                  '��������?
1682                     M_Out(MOUT_IS_Insp%) = 1                    '�������s�v��on
1683                     '�捞�����m�F
1684                     M_Timer(1) = 0
1685                     MExitFlg = 0
1686                     While( MExitFlg = 0 )
1687                         '���������҂�
1688                         If M_In( MIN_IS_InspCapDone% ) = 1  Then
1689                             MExitFlg = 1
1690                         EndIf
1691                         'timeout�`�F�b�N
1692                         If 2000 < M_Timer(1) Then
1693                             If MNgContinue% = 1 Then            'NG���s?
1694                                 MInspErrNumSub = 33             '�G���[�ԍ��ݒ�33
1695                             Else
1696                                 MInspErrNum = 33                '�G���[�ԍ��ݒ�33
1697                             EndIf
1698                             MExitFlg = 1
1699                         EndIf
1700                     WEnd
1701                 EndIf
1702                 '
1703             EndIf
1704         EndIf
1705         MNum% = MNum% + 1
1706     WEnd
1707     '
1708     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
1709     If 0 < MZAxis% Then
1710         PCurrentPos = P_Curr                                    '���݈ʒu�擾
1711         PCurrentPos.Z = MZAxis%                                 'Z����ݒ�
1712         Mvs PCurrentPos                                         '���݈ʒu���ֈړ�
1713     EndIf
1714     '
1715     'NG���s������
1716     If MNgContinue% = 1 Then                                    'NG���s?
1717         MInspErrNum = MInspErrNumSub                            '�G���[�ԍ��ݒ�
1718     EndIf
1719     '
1720     '�߂�l�ݒ�
1721     If MInspErrNum = 0 Then
1722         ISInspection = 1                                        '����I���߂�l�ݒ�
1723     Else
1724         M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
1725         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
1726         ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
1727     EndIf
1728     Fine 0 , P
1729     '
1730 *ISInspection_End
1731 FEnd
1732 '
1733 '��fnAutoScreenComment
1734 ''' <summary>
1735 ''' ���C����ʂ̓���󋵕\��
1736 ''' �R�����gD1005�̐ݒ�
1737 ''' </summary>
1738 '''<param name="McommentD1005%">�R�����gID</param>
1739 ''' <remarks>
1740 ''' Date   : 2021/07/07 : M.Hayakawa
1741 ''' </remarks>
1742 Function fnAutoScreenComment(ByVal McommentD1005%)
1743     M_Out16(12576) = McommentD1005%
1744 FEnd
1745 '
1746 '��fnRoboPosChk
1747 ''' <summary>
1748 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
1749 ''' </summary>
1750 '''<param name="MINNumber%">���͔ԍ�</param>
1751 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
1752 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
1753 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
1754 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
1755 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
1756 ''' <remarks>
1757 ''' Date   : 2021/07/07 : M.Hayakawa
1758 ''' </remarks>
1759 Function M% fnRoboPosChk
1760     fnRoboPosChk = 0
1761     MRet = fnStepRead()
1762     '�����ʒu�łȂ��Ɣ��f�����ꍇ
1763     '�E�B���h��ʐ؊���
1764     If MRBTOpeGroupNo > 5 Then
1765         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
1766         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
1767         Dly 0.2
1768         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
1769         Dly 1.5
1770         '
1771         MRet = fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
1772         '
1773         MLoopFlg% = 1
1774         While MLoopFlg% = 1
1775             '
1776             '
1777             MKeyNumber% = fnKEY_WAIT()
1778             Select MKeyNumber%
1779                 Case Is = MAbout%       '��~
1780                     M_20# = MAbout%
1781                     MLoopFlg% = -1
1782                     Break
1783                 Case Is = MNext%        '����
1784                     'MLoopFlg% = -1
1785                     Break
1786                 Case Is = MContinue%    '�p��
1787                     M_20# = MContinue%
1788                     MLoopFlg% = -1
1789                     Break
1790                 Default
1791                     Break
1792             End Select
1793         WEnd
1794     EndIf
1795     '
1796     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
1797         MRet = fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
1798         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
1799         Select MRBTOpeGroupNo
1800             Case Is = 5                          '�������Ȃ�
1801                 Break
1802             Case Is = 10                         '�����ʒu�֖߂�
1803                 'Mov PTEST001
1804                 Break
1805             Case Is = 15                         '�����ʒu�֖߂�
1806                 'Mov PTEST002
1807                 Dly 0.5
1808                 'Mov PTEST001
1809                 Dly 0.5
1810                 Break
1811             Default
1812                 Break
1813         End Select
1814         '
1815         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
1816         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
1817         MRBTOpeGroupNo = 5
1818         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
1819         Dly 1.0
1820         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
1821         fnRoboPosChk = 1                        '�����ʒu������s
1822         MRet = fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
1823     EndIf
1824     Exit Function
1825 FEnd
1826 '
1827 '��frInCheck
1828 ''' <summary>
1829 ''' �Z���T�[IN�`�F�b�N
1830 ''' </summary>
1831 '''<param name="MINNumber%">���͔ԍ�</param>
1832 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
1833 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
1834 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
1835 ''' <remarks>
1836 ''' Date   : 2021/07/07 : M.Hayakawa
1837 ''' </remarks>
1838 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
1839     M_Timer(4) = 0
1840     MloopFlg = 0
1841     While MloopFlg = 0
1842         MCrtTime& = M_Timer(4)
1843         If M_In(MINNumber%) = MCMPFLG% Then
1844             MloopFlg = 1
1845             frInCheck = 1
1846         ElseIf MCrtTime& > MTimeCnt& Then
1847             MloopFlg = 1
1848             frInCheck = 0
1849         EndIf
1850     WEnd
1851 FEnd
1852 '-----------------------------------------------
1853 '
1854 '�˂����ߋ@�ʐM�m�F
1855 '
1856 '-----------------------------------------------
1857 Function M% fScewTcomChk
1858     fScewTcomChk = 0
1859     '�ʐM�m�F���M
1860     M_Out(MOUT_ScwT_ComChk%) = MOn%
1861     '�ʐM�m�F��M�ҋ@
1862     Wait M_In(MIN_ScwT_comOK%) = MOn%
1863     '�ʐM�m�F���M�I��
1864     M_Out(MOUT_ScwT_ComChk%) = MOff%
1865  '
1866 FEnd
1867 '
1868 '
1869 '-----------------------------------------------
1870 '
1871 '�˂����ߊJ�n���M
1872 '
1873 '-----------------------------------------------
1874 Function M% fScewTStart
1875     fScewTStart = 0
1876     '�˂����ߊJ�n�ҋ@����M
1877     Wait M_In(MIN_ScwT_STRec%) = MOn%
1878     Dly 0.1
1879     '�˂����ߊJ�n��M�𑗐M
1880     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msec�p���X
1881 FEnd
1882 '
1883 '
1884 '-----------------------------------------------
1885 '
1886 '�˂����ߊ�����M
1887 '
1888 '-----------------------------------------------
1889 Function M% fScewTFinish
1890     fScewTFinish = 0
1891     '�˂����ߊ����ҋ@����M
1892     Wait M_In(MIN_ScwT_Fin%) = MOn%
1893     Dly 0.1
1894     '�˂����ߊ�����M�𑗐M
1895     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msec�p���X
1896 FEnd
1897 '
1898 '
1899 '-----------------------------------------------
1900 '
1901 '����xx��~��M
1902 '
1903 '-----------------------------------------------
1904 Function M% fScewTCaseStop(ByVal MCase%())
1905     fScewTCaseStop = 0
1906     '����xx��~����M
1907     Wait M_In(MCase%(1)) = MOn%
1908     Dly 0.1
1909     '����xx��~��M�𑗐M
1910     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msec�p���X
1911 FEnd
1912 '
1913 '-----------------------------------------------
1914 '
1915 '�ĊJ�n��M
1916 '
1917 '-----------------------------------------------
1918 Function M% fScewTReStart()
1919     fScewTReStart = 0
1920     '�ĊJ�n����M
1921     Wait M_In(MIN_ScwT_ReST%) = MOn%
1922     Dly 0.1
1923     '�ĊJ�n��M�𑗐M
1924     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msec�p���X
1925 FEnd
1926 '
1927 '��fErrorProcess
1928 '<summary>
1929 '�G���[����
1930 '</summary>
1931 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
1932 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
1933 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
1934 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
1935 '<make>
1936 '2021/11/5 �����V��
1937 '</make>
1938 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
1939     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
1940     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
1941     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
1942     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
1943 *RETRY_ERR_PROCESS
1944      M_20# = MClear%     '������
1945 '        '�G���[�����L�q
1946         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
1947 '        'GOT KEY���͑҂�
1948         MKeyNumber = fnKEY_WAIT()
1949 '        '
1950         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1951             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1952             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1953             Break
1954          '
1955         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1956             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1957             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1958         '
1959         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1960             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1961             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1962          '
1963         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1964             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1965             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1966             Break
1967         '
1968         EndIf
1969         '
1970         If M_20# = MClear% Then *RETRY_ERR_PROCESS
1971 FEnd
1972 '
1973 '��Main
1974 ''' <summary>
1975 ''' �g������p�̃��C��
1976 ''' </summary>
1977 ''' <remarks>
1978 ''' Date   : 2021/07/07 : M.Hayakawa
1979 ''' </remarks>'
1980 Function Main
1981     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
1982     '
1983     If M_Svo=0 Then
1984         Servo On
1985     EndIf
1986     Wait M_Svo=1
1987 '�g���X�^�[�g���t�����v���p���XON
1988     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
1989 '�p�g���C�g����
1990     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
1991     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
1992     '
1993     M_20# = 0                                   'KEY���͏�����
1994     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
1995     MRet% = 0
1996 '�����ʒu�̊m�F�ƈړ�
1997 '    MRet% = fnRoboPosChk()
1998     If MRet% = 1 Then                           '�����ʒu�̓�����s�����ꍇ
1999         MRet% = fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  '�E�B���h���
2000         MKeyNumber% = fnKEY_WAIT()
2001         Select MKeyNumber%
2002             Case Is = MAbout%       '��~
2003                 M_20# = MAbout%
2004                 MLoopFlg% = -1
2005                 Break
2006             Case Is = MNext%        '����
2007                 'MLoopFlg = -1
2008                 Break
2009             Case Is = MContinue%    '�p��
2010                 M_20# = MContinue%
2011                 MLoopFlg% = -1
2012                 Break
2013             Default
2014                 Break
2015         End Select
2016     EndIf
2017     '
2018     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
2019         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
2020 '�g���N�`�F�b�N
2021         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
2022             MRet% = fnTorqueCheck()
2023             Break
2024         Else
2025 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
2026 '                MRtn = InspInit()               '�摜��������������
2027 '            EndIf
2028 '�������A�g��OK���A�g��NG��Read/Write
2029             fnCtlValue(1)               '�Ǎ���
2030             nInputQty = nInputQty + 1   '�������C���N�������g
2031             fnCtlValue(2)               '������
2032             '
2033            M_20# = MClear%                    '������
2034 '�g���J�n
2035             If M_In(MIN_ASSY_CANCEL%) = 0 Then
2036 '                MRet% = fnAssyStart()
2037                 fnAssyStart()
2038             Else
2039                 M_20# = MPass%
2040             EndIf
2041 '�g���I�����t����
2042             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
2043             Wait M_In(11572) = 1            '���t�擾����
2044             Dly 0.1
2045             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
2046 '���t�^�[���j�b�g�ւ�OUT
2047             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
2048             fnAutoScreenComment(89)         'AUTO��� �g����������
2049             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
2050 'OK/NG�t���O�o��
2051             If M_20# <= 0 Then
2052                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
2053             ElseIf M_20# = MPass% Then
2054                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
2055             EndIf
2056 'PIAS�ɑg������������
2057             If M_In(MIN_PIAS_Use%) = 1 Then             'PIAS_ON�m�F
2058                 If M_20# <> MPass% Then
2059                     'KEY���͂�NG�̏ꍇ
2060                     If M_20# = MNgProcess% Then
2061                         M_Out(MOUT_OKNG%) = 0           '��H����NG�t���O���o��(PLC OUT)
2062                         'MRet% = fnAutoScreenComment(90) 'AUTO��� �ʉߗ���NG������
2063                         'MRet% = fnPiasWrite(MNG%)
2064                        nAssyNgQty = nAssyNgQty + 1      ' �g��NG���C���N�������g
2065                     EndIf
2066                     '
2067                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f
2068                     If M_20# <= 0 Then
2069                             'D732 -> D2600 �R�s�[�v��
2070                             M_Out(12566) = 1
2071 '                            Wait M_In(11581) = 1        'PLC���R�s�[�����M��
2072                             M_Out(12566) = 0
2073                             '
2074                         If M_In(11367) = 0 Then         '����������݃L�����Z��=1 DEbug�p
2075                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
2076                             '��ԍ��ƍ�
2077                             MRet% = fnPCBNumberCheck()
2078                         Else
2079                             MRet% = 1
2080                         EndIf
2081                         '
2082                         If M_In(11368) = 0 Then         '�H�����������݃L�����Z��=1 DEbug�p
2083                             If M_20# <> MAbout% Then
2084                                 '�H������OK��������
2085                                 M_Out(MOUT_OKNG%) = 1   '��H����OK�t���O���o��(PLC OUT)
2086                                 'MRet% = fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
2087                                 'MRet% = fnPiasWrite(MOK%)
2088                                 nAssyOkQty = nAssyOkQty + 1
2089                             Else
2090                                 nAssyOkQty = nAssyOkQty + 1
2091                             EndIf
2092                         EndIf
2093                     EndIf
2094                 Else
2095                     M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
2096                 EndIf
2097             Else
2098                 nAssyOkQty = nAssyOkQty + 1             ' �g��OK���C���N�������g
2099             EndIf
2100             '
2101             '�g���I�����t��������
2102             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
2103             '�������A�g��OK���A�g��NG��������
2104             fnCtlValue(2)                       '������
2105             '
2106 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
2107 '                '�摜�����I������
2108 '                MRtn = InspQuit()
2109 '            EndIf
2110         EndIf
2111         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
2112     EndIf
2113 '�p�g���C�g����
2114     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
2115     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
2116 'GOT�\��
2117     fnAutoScreenComment(93)  'AUTO��� �H������
2118 FEnd
2119 End
2120 '
2121 '���܂��Ȃ��R�����g
2122 '��΍폜�����
2123 '
2124 '
PInspPosition(1)=(+601.00,-152.00,+375.00,+180.00,+0.00,+90.00,+0.00,+0.00)(7,0)
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
PTemp=(+601.01,-151.96,+429.95,-179.99,+0.00,+90.00,+0.00,+0.00)(7,0)
PScrewPos(1)=(-377.09,+108.11,+620.00,+180.00,+0.00,+0.00,+0.00,+0.00)(7,0)
PScrewPos(2)=(-377.09,+108.11,+577.47,+180.00,+0.00,+0.00,+0.00,+0.00)(7,0)
PScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(9)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PScrewPos(10)=(-377.10,+108.11,+567.73,+180.00,+0.00,+0.00,+0.00,+0.00)(7,0)
PGetScrewPos(1)=(-282.57,-243.43,+422.84,-179.99,+0.00,-58.70,+0.00,+0.00)(7,1)
PGetScrewPos(2)=(-147.31,-147.28,+610.00,-180.00,+0.00,-45.01,+0.00,+0.00)(7,1)
PGetScrewPos(3)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(4)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(5)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(6)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(7)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(8)=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PGetScrewPos(9)=(-235.40,-243.41,+334.56,-179.99,+0.00,-58.70,+0.00,+0.00)(7,1)
PGetScrewPos(10)=(-282.57,-243.44,+403.39,-179.99,+0.00,-58.70,+0.00,+0.00)(7,1)
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
PEscapePosition=(+247.61,-0.38,+580.00,-180.00,+0.00,-179.99)(7,0)
PEscapePosition_2=(-131.77,+177.64,+579.99,-180.00,+0.00,-53.32)(7,0)
PEscapePosition_3=(-178.95,+226.16,+579.83,-180.00,-0.02,-53.33)(7,0)
PEscapePosition_4=(-221.18,+0.03,+579.99,-180.00,+0.00,+0.10)(7,0)
PInitialPosition=(+300.00,+0.00,+440.00,-180.00,+0.00,-180.00)(7,0)
PProductOnJigGet=(-245.32,-0.38,+429.95,-49.33,+88.91,+130.56)(6,1)
PProductOnJigGet_1=(-245.33,-0.40,+460.00,-48.95,+88.90,+130.94)(6,1)
PProductOnJigGet_2=(-190.37,-0.40,+560.00,-48.95,+88.90,+130.94)(6,1)
PProductOnJigGet_3=(-133.01,+133.47,+580.00,-173.13,+89.99,-38.35)(6,0)
PProductOnJigGet_4=(-164.65,+0.00,+671.53,-166.66,+90.00,+13.34)(7,0)
PProductOnJigGet_5=(-224.30,+0.01,+604.77,+180.00,+0.00,+0.00)(7,0)
PProductOnJigGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnJigSet=(-245.32,-0.40,+430.31,-49.37,+88.91,+130.52)(6,1)
PProductOnJigSet_1=(-245.33,-0.40,+460.00,-48.95,+88.90,+130.94)(6,1)
PProductOnJigSet_2=(-190.37,-0.40,+580.00,-48.95,+88.90,+130.94)(6,1)
PProductOnJigSet_3=(-136.17,+133.06,+580.00,-175.95,+89.99,-39.47)(6,0)
PProductOnJigSet_4=(+163.76,+97.11,+580.00,-176.54,+89.98,-145.04)(6,0)
PProductOnPltGet=(+549.98,-99.42,+250.75,+180.00,+0.00,-179.03)(7,0)
PProductOnPltGet_1=(+549.98,-99.42,+290.00,+180.00,+0.00,-179.03)(7,0)
PProductOnPltGet_2=(+549.98,-99.42,+400.00,+180.00,+0.00,-179.03)(7,0)
PProductOnPltSet=(+548.99,-100.57,+251.32,+180.00,+0.00,-179.54)(7,0)
PProductOnPltSet_1=(+548.99,-100.57,+290.00,+180.00,+0.00,-179.54)(7,0)
PProductOnPltSet_2=(+548.99,-100.57,+400.00,+180.00,+0.00,-179.54)(7,0)
PProductOnPltSet_3=(+133.46,+133.02,+580.00,-173.90,+89.99,-129.11)(6,0)
PScrewHeatSink1=(-377.09,+81.39,+568.02,+180.00,+0.00,+0.00)(7,0)
PScrewHeatSink1_0=(-377.09,+81.39,+577.45,+180.00,+0.00,+0.00)(7,0)
PScrewHeatSink1_1=(-377.09,+81.39,+620.00,+180.00,+0.00,+0.00)(7,0)
PScrewHeatSink2=(-377.10,+108.11,+567.73,+180.00,+0.00,+0.00)(7,0)
PScrewHeatSink2_0=(-377.09,+108.11,+577.47,+180.00,+0.00,+0.00)(7,0)
PScrewHeatSink2_1=(-377.09,+108.11,+620.00,+180.00,+0.00,+0.00)(7,0)
PScrewPlateL=(-284.78,+112.54,+536.90,-180.00,+0.00,-90.00)(7,0)
PScrewPlateL1=(-317.11,+26.69,+536.90,+179.96,+0.00,-90.00)(7,0)
PScrewPlateL1_0=(-317.11,+26.71,+543.53,+179.96,+0.00,-90.00)(7,0)
PScrewPlateL1_1=(-317.11,+26.71,+570.00,+179.96,+0.00,-90.00)(7,0)
PScrewPlateL2=(-317.26,+101.98,+536.35,-180.00,+0.00,-90.00)(7,0)
PScrewPlateL2_0=(-317.26,+101.99,+543.71,-180.00,+0.00,-90.00)(7,0)
PScrewPlateL2_1=(-317.26,+101.99,+570.00,-180.00,+0.00,-90.00)(7,0)
PScrewPlateL_0=(-284.78,+112.54,+543.53,-180.00,+0.00,-90.00)(7,0)
PScrewPlateL_1=(-284.78,+112.54,+570.00,-180.00,+0.00,-90.00)(7,0)
PScrewPlateR=(-286.12,-114.33,+536.39,-180.00,+0.00,-90.00)(7,1)
PScrewPlateR1=(-317.31,-28.86,+536.39,-180.00,+0.00,-89.94)(7,1)
PScrewPlateR1_0=(-317.31,-28.86,+543.60,-180.00,+0.00,-89.94)(7,1)
PScrewPlateR1_1=(-317.31,-28.85,+569.98,-180.00,+0.00,-89.94)(7,1)
PScrewPlateR2=(-318.02,-104.08,+536.32,-179.99,+0.00,-89.94)(7,1)
PScrewPlateR2_0=(-318.03,-104.06,+543.56,-179.99,+0.00,-89.94)(7,1)
PScrewPlateR2_1=(-318.02,-104.08,+570.00,-179.99,+0.00,-89.94)(7,1)
PScrewPlateR_0=(-286.12,-114.33,+543.60,-180.00,+0.00,-90.00)(7,1)
PScrewPlateR_1=(-286.12,-114.33,+570.00,-180.00,+0.00,-90.00)(7,1)
PScrewSupplyHS=(-282.57,-243.44,+403.39,-179.99,+0.00,-58.70)(7,1)
PScrewSupplyHS_1=(-282.57,-243.43,+422.84,-179.99,+0.00,-58.70)(7,1)
PScrewSupplyHS_2=(-147.31,-147.28,+610.00,-180.00,+0.00,-45.01)(7,1)
PScrewSupplyHS_3=(-235.18,-0.02,+609.98,+180.00,+0.00,+0.00)(7,1)
PScrewSupplyHS_4=(-235.40,-243.41,+334.56,-179.99,+0.00,-58.70)(7,1)
PScrewSupplyPlate=(-136.35,+200.79,+452.16,-180.00,+0.01,+170.74)(7,0)
PScrewSupplyPlate_1=(-136.35,+200.79,+470.00,-180.00,+0.01,+170.74)(7,0)
PScrewSupplyPlate_2=(-136.35,+200.79,+570.00,-180.00,+0.01,+170.74)(7,0)
PScrewSupplyPlate_3=(-132.53,+160.72,+570.00,-179.99,+0.00,-140.43)(7,0)
PScrewSupplyPlate_4=(-208.31,+0.23,+610.00,-180.00,+0.00,-90.00)(7,0)
PScrewSupplyPlate_5=(-113.97,+113.99,+696.72,-0.32,+89.24,+134.68)(7,0)
PScrewSupplyPlate_6=(-161.19,+0.02,+696.72,-0.32,+89.24,+179.67)(7,0)
PScrewSupplyPlate_7=(-50.58,+239.35,+544.69,-180.00,+0.01,+170.74)(7,0)
PScrewSupplyPlatel_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(,)
PTicketRead=(+601.00,-152.00,+375.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+601.00,-152.00,+430.00,-180.00,+0.00,+90.00)(7,0)
