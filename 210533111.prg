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
214 Def Inte nAssyOkQty         '���g�p
215 Def Inte MScrewNo
216 Def Inte MReTry
217 '===== <IO�ϐ���`> =====
218 Def Inte MIN_VS1            ' �A�[����[�@�l�W�z���Z���T1
219 'Def Inte MIN_VS2           ' �A�[����[�@�l�W�z���Z���T2�@���@�A�C�I�[�_������Ȃ����ߔp�~
220 Def Inte MIN_CS13           ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
221 Def Inte MIN_CS1            ' �A�[����[�@MainPWB�p�`���b�N���o
222 Def Inte MIN_CS2            ' �A�[����[�@MainPWB�p�`���b�N�J���o
223 Def Inte MIN_CS3            ' �A�[����[�@�T�u�V���V�p�`���b�N���o
224 Def Inte MIN_CS4            ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
225 Def Inte MIN_PSE1           ' �A�[����[�@���[�N���o���dSW
226 '
227 Def Inte Y6A_VV1            ' �A�[����[�@�l�W�z���o���u
228 Def Inte Y6B_VB1            '�A�[����[�@�z���j��o���u
229 Def Inte MOUT_VB1           ' �A�[����[�@�l�W�z���j��o���u
230 '
231 Def Inte MIN_CS5            ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
232 Def Inte MIN_CS6            ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
233 Def Inte MIN_CS7            ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
234 Def Inte MIN_CS8            ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
235 Def Inte MIN_CS9            ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
236 Def Inte MIN_CS10           ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
237 Def Inte MIN_CS11           ' �x�[�X���@�N�����vCy�ߒ[ ���o
238 Def Inte MIN_CS12           ' �x�[�X���@�N�����vCy�o�[ ���o
239 Def Inte MIN_PSE2           ' �x�[�X���@�@�픻�ʃZ���T1
240 Def Inte MIN_PSE3           ' �x�[�X���@�@�픻�ʃZ���T2
241 '
242 Def Inte MOUT_SV9           ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
243 Def Inte MOUT_SV10          ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
244 Def Inte MOUT_SV11          ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
245 '
246 Def Inte MOUT_LED1          ' �摜�����pLED�Ɩ�
247 '
248 Def Inte MNEJI_COUNTS       ' �˂����߂�{���J�E���g�A�b�v�p�ϐ�
249 Def Inte MNEJI_G_ERR_COUNTS ' �˂������A���G���[�J�E���g�A�b�v�p�ϐ�
250 '
251 Def Inte MSTORE_INP_ADD     '�@���͎��ԊĎ��Ώۂ̃A�h���X�����
252 Def Inte MCOUNT_UP_SEC      '�@�Z���T����WaitTimer�̃J�E���^�[�@msec
253 Def Inte MCOUNT_UP_LIM      '�@�Z���T����WaitTimer�̃J�E���g�A�b�v���ԁ@msec
254 Def Inte MCOUNT_UP_JUDG     '�@�Z���T����WaitTimer�̖߂蔻��l�@0��NG�@1��OK�@2���J�E���g�A�b�v��
255 Def Inte MCHUCK_RET_COUNTS  '  �`���b�L���O�E�A�����g���C�E�J�E���g�A�b�v�p�ϐ�
256 Def Inte MCLUMP_RET_COUNTS  '  �T�u�V���V�E�N�����v�E�A�����g���C�J�E���g�A�b�v�p�ϐ�
257 '
258 Def Inte MOUT_Y7E_BACKUP    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
259 Def Inte MIN_X32_BACKUP_IN  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
260 Def Inte MIN_X33_BACKUP_OUT '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
261 '
262 MIN_VS1%    =  11259    ' �A�[����[�@�l�W�z���Z���T1
263 MIN_CS13%   =  11260    ' �A�[����[�@�V���V�E�T�|�[�gCy�ߒ[�@���o
264 MIN_CS1%    =  11261    ' �A�[����[�@MainPWB�p�`���b�N���o
265 MIN_CS2%    =  11262    ' �A�[����[�@MainPWB�p�`���b�N�J���o
266 MIN_CS3%    =  11263    ' �A�[����[�@�T�u�V���V�p�`���b�N���o
267 MIN_CS4%    =  11264    ' �A�[����[�@�T�u�V���V�p�`���b�N�J���o
268 MIN_PSE1%   =  11265    ' �A�[����[�@���[�N���o���dSW
269 Y6A_VV1%    =  12250    ' �A�[����[�@�l�W�z���o���u
270 Y6B_VB1%    =  12251    '�A�[����[�@�z���j��o���u
271 MOUT_VB1%   =  12251    ' �A�[����[�@�l�W�z���j��o���u
272 '
273 MIN_CS5%    =  11269    ' �x�[�X���@SubChassis�v�b�V��Cy�ߒ[�@���o
274 MIN_CS6%    =  11270    ' �x�[�X���@SubChassis�v�b�V��Cy�o�[�@���o
275 MIN_CS7%    =  11271    ' �x�[�X���@�X���C�hL�Cy�ߒ[ ���o
276 MIN_CS8%    =  11272    ' �x�[�X���@�X���C�hL�Cy�o�[ ���o
277 MIN_CS9%    =  11273    ' �x�[�X���@�X���C�hR�Cy�ߒ[ ���o
278 MIN_CS10%   =  11274    ' �x�[�X���@�X���C�hR�Cy�o�[ ���o
279 MIN_CS11%   =  11275    ' �x�[�X���@�N�����vCy�ߒ[ ���o
280 MIN_CS12%   =  11276    ' �x�[�X���@�N�����vCy�o�[ ���o
281 MIN_PSE2%   =  11277    ' �x�[�X���@�@�픻�ʃZ���T1
282 MIN_PSE3%   =  11278    ' �x�[�X���@�@�픻�ʃZ���T2
283 '
284 MOUT_SV9%   =  12267    ' �x�[�X���@�v�b�V��Cy�pSV(on�ňʒu���ߕ���)
285 MOUT_SV10%  =  12268    ' �x�[�X���@�X���C�hLR�Cy�pSV(on�ňʒu���ߕ���)
286 MOUT_SV11%  =  12269    ' �x�[�X���@MainPWB�����グ�h�~Cy�pSV
287 '
288 MOUT_LED1%  =  12239    ' �摜�����pLED�Ɩ�
289 '
290 MOUT_Y7E_BACKUP% = 12270    '  �T�u�V���[�V�ό`�΍􎡋� 2020-02-06
291 MIN_X32_BACKUP_IN% = 11267  '  �T�u�V���[�V�ό`�΍􎡋� �߂�Z���T�[2020-02-06
292 MIN_X33_BACKUP_OUT% = 11266 '  �T�u�V���[�V�ό`�΍􎡋� �o�Z���T�[2020-02-06
293 '
294 '����
295 Def Inte MTEST_KEY                      '�f�o�b�N�e�X�g�p
296 Def Inte MOn                            '�o��=1
297 Def Inte MOff                           '�o��=0
298 '
299 '�˂����ߑ��u_�o�̓A�h���X
300 Def Inte MOUT_ScwT_ComChk               '�ʐM�m�F
301 Def Inte MOUT_ScwT_ST                   '�˂����ߊJ�n
302 Def Inte MOUT_ScwT_FinOK                '�˂����ߊ�����M�𑗐M
303 Def Inte MOUT_ScwT_Case1OK              '����1��~��M�𑗐M
304 Def Inte MOUT_ScwT_Case2OK              '����2��~��M�𑗐M
305 Def Inte MOUT_ScwT_Case3OK              '����3��~��M�𑗐M
306 Def Inte MOUT_ScwT_Case4OK              '����4��~��M�𑗐M
307 Def Inte MOUT_ScwT_Case5OK              '����5��~��M�𑗐M
308 '�˂����ߑ��u_���̓A�h���X
309 Def Inte MIN_ScwT_comOK                 '�ʐM�m�F�ԐM
310 Def Inte MIN_ScwT_STRec                 '�˂����ߊJ�n����M
311 Def Inte MIN_ScwT_Fin                   '�˂����ߊ�������M
312 Def Inte MIN_ScwT_Case1                 '����1��~����M
313 Def Inte MIN_ScwT_Case2                 '����2��~����M
314 Def Inte MIN_ScwT_Case3                 '����3��~����M
315 Def Inte MIN_ScwT_Case4                 '����4��~����M
316 Def Inte MIN_ScwT_Case5                 '����5��~����M
317 '
318 Dim MScwT_Case1%(2)               '����1��~�ϐ�
319 Dim MScwT_Case2%(2)               '����2��~�ϐ�
320 Dim MScwT_Case3%(2)               '����3��~�ϐ�
321 Dim MScwT_Case4%(2)               '����4��~�ϐ�
322 Dim MScwT_Case5%(2)               '����5��~�ϐ�
323 '
324 '����
325 MTEST_KEY% = 11359                       '�f�o�b�O�p�e�X�gKEY
326 MOn% = 1                                 '�o�� = 1
327 MOff% = 0                                '�o�� = 0
328 '
329 '�˂����ߋ@_�A�h���X�ݒ�
330 MOUT_ScwT_ComChk% = 12832               '�ʐM�m�F���M
331 MOUT_ScwT_ST% = 12865                   '�˂����ߊJ�n�𑗐M
332 MOUT_ScwT_ReSTOK% = 12866               '�ĊJ�n��M�𑗐M
333 MOUT_ScwT_FinOK% = 12868                '�˂����ߊ�����M�𑗐M
334 MOUT_ScwT_Case1OK% = 12874              '����1��~��M�𑗐M
335 MOUT_ScwT_Case2OK% = 12875              '����2��~��M�𑗐M
336 MOUT_ScwT_Case3OK% = 12876              '����3��~��M�𑗐M
337 MOUT_ScwT_Case4OK% = 12877              '����4��~��M�𑗐M
338 MOUT_ScwT_Case5OK% = 12878              '����5��~��M�𑗐M
339 '
340 MIN_ScwT_comOK% = 11840                 '�˂����ߑ��u����ԐM
341 MIN_ScwT_STRec% = 11873                 '�˂����ߊJ�n����M
342 MIN_ScwT_ReST% = 11874                  '�ĊJ�n����M
343 MIN_ScwT_Fin% = 11876                   '�˂����ߊ�������M
344 MIN_ScwT_Case1% = 11882                 '����1��~�ҋ@����M
345 MIN_ScwT_Case2% = 11883                 '����2��~�ҋ@����M
346 MIN_ScwT_Case3% = 11884                 '����3��~�ҋ@����M
347 MIN_ScwT_Case4% = 11885                 '����4��~�ҋ@����M
348 MIN_ScwT_Case5% = 11886                 '����5��~�ҋ@����M
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
361 '�ݒ� InitialZoneB�Ŏg�p����ϐ�
362 Def Pos PActive       '�������W�n �ʒu�ϐ� ���݈ʒu
363 Def Pos Pmove         '�������W�n �ʒu�ϐ� �ړ���
364 Def Jnt JActive       '�֐ߍ��W�n �ʒu�ϐ� ���݈ʒu
365 Def Jnt Jmove         '�֐ߍ��W�n �ʒu�ϐ� �ړ���
366 Def Jnt JTaihi        '�֐ߍ��W�n �ʒu�ϐ� �ޔ��|�W�V���� �e�B�[�`���O�Őݒ�
367 Def Inte MRecoveryPass      '���A����p�X�t���O�@1=���A������p�X�@0=���A��������s
368 Def Inte MJ6          'J6���̒l���r����ׂ̕ϐ�
369 Def Inte MStandby              '�ҋ@�ʒu�m�F�t���O
370 Def Inte MRecoveryChuckOpen    '�`���b�N����t���O�i���A����O�j���͂ݑ΍�
371 '�����Ӂ������ʒu��ǉ��ύX�������ɂ́A�ύX���K�v�I
372 '
373 'Function�G���[�΍�
374 Def Inte MCtlNo
375 '
376 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
377 Function M% fnAssyStart
378     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
379 '   BaseUnit6�ʐM�m�F
380     *RE_COM_CHECK
381     MRtn = 1    '������
382     If M_In(11920) = 0 Then     'BaseUnit6���E�����̃t���O�𗧂ĂĂ��Ȃ�
383         If M_In(11930) = 0 And M_In(11931) = 0 Then   '�ʐM�ɂĉ�]�p�s��
384             Dly 2.3                                   '��]�҂�
385             If M_In(11930) = 0 And M_In(11931) = 0 Then  '������x�m�F
386                 MRtn = 0                              '�ʐM�ɂĈُ�
387                 Break
388             EndIf
389             Break
390         EndIf
391         Break
392     EndIf
393     If MRtn = 1 Then GoTo *BU6Com_OK    '�ʐMOK�Ȃ烉�x���W�����v
394     fErrorProcess(11,298,284,0)           '0,284��298,287�ɕύX6/7����
395     If M_20# = MNext% Then M_20# = MClear%
396     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
397     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
398     If M_20# = MContinue% Then GoTo *RE_COM_CHECK
399     *BU6Com_OK
400 '
401 '
402 ' PIAS�`�P�b�g�Ǎ��ݍH�������m�F
403     M_20# = MClear%                       '������
404 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
405 '        MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
406 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
407 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
408 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' ���j���[�֖߂�
409 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NG���H�������ɏ����ݎ��̍H����
410 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NG���H�������ɏ����ݎ��̍H����
411 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' ����NG, �H������
412 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK�ȊO�͑g���I��
413 '    EndIf
414 '    '
415 '    '���W�ړ�
416 '    '
417 '    '����xx��~
418 '    fScewTCaseStop(MScwT_Case5%)
419 '    '
420 '    '�x�[�X���j�b�gKEY
421 '    Wait M_In(MTEST_KEY%) = MOn%
422 '    '
423 '    '�ĊJ�n
424 '    fScewTReStart()
425 '    '
426 '    '���W�ړ�
427 '    '
428 '    '�˂����ߊ���
429 '    Mret% = fScewTFinish()
430 ' �l�W���߃e�X�g�I��
431 ' PIAS�e�X�g -----------
432 '    MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
433 '    MRet% = fnPiasWrite(MNG%)
434  '   MRet% = fnPCBNumberCheck()
435 ' PIAS�e�X�g�I�� -------
436 '
437     '�g���J�n(9/6�ǉ�(����))
438     '�v���O�������_
439     Ovrd 100
440     ' �n���h��ԏ�����(10/29�ǉ�M.H)(2/11�C��(����))
441     Cmp Off                     '�R���v���C�A���X���[�h�I��
442     ColChk On                   '�Փˌ��mON
443     If M_In(11266) Then
444         M_Out(12256) = 0
445         M_Out(12257) = 1
446     EndIf
447     If M_In(11269) Then
448         M_Out(12258) = 0
449         M_Out(12259) = 1
450     EndIf
451     If M_In(11271) Then
452         M_Out(12260) = 0
453         M_Out(12261) = 1
454     EndIf
455     *WAIT_HAND_INI
456     If M_In(11265) = 1 And M_In(11268) = 1 And M_In(11270) = 1 Then GoTo *CompHandIni Else GoTo *WAIT_HAND_INI
457     *CompHandIni
458     M_Out(12257) = 0
459     M_Out(12259) = 0
460     M_Out(12261) = 0
461 '
462 '
463 'Dly 5                               '�f�o�b�O�p(22/09/30����)
464     ' �˂����ߋ@�e�X�g�p ----------
465     Mret% = fScrewTcomChk()
466     If Mret% = -1 Then GoTo *ASSY_ERROR_END
467     ' �˂����ߋ@�ʐM�J�n
468 '    fScrewTStart()           '�b��R�����g�A�E�g����(10/19����)
469     '�`�P�b�gID��ǂ�
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
538     '�G���[�����i�ʒu���߂�����
539 *RE_ERR_REL_1
540 If M_20# = MContinue% Then M_20# = MRtn
541 M_Out(12263) = 1 Dly 0.5    '���i�ʒu���߂�����
542 MRtn = frInCheck(11274,1,MSETTIMEOUT05&)
543 '
544 If MRtn = 1 Then GoTo *CompErrorRelease
545 MRtn = M_20#        'M_20#�ꎞ���
546 M_20# = MClear%
547 fErrorProcess(11,234,284,0)     '�ʒu���ߖߒ[�G���[
548 If M_20# = MContinue% Then GoTo *RE_ERR_REL_1
549 If M_20# = MNext% Then M_20# = MRtn
550 If M_20# = MNgProcess% Then M_20# = MAbout%
551 *CompErrorRelease
552 '
553 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
554 If M_20# = MNext% Then M_20# = MPass%
555 Mvs PTicketRead_1                         '22/04/07 �ǉ� �n��
556 GoTo *ASSY_ERROR_END
557 *CompRead
558 '
559 '    �p���b�g���琻�i�����
560     fScrewTStart()           '�l�W���ߊJ�n
561 '
562     Mov PProductOnPltGet_2      '�{�̎󂯎������_
563     M_Out(12256) = 0            '�{�̃`���b�N��OFF(�����ʒu�ύX2/11����)
564     M_Out(12257) = 1            '�{�̃`���b�N�JON
565 '
566     *RE_POSITIONING
567     '
568     M_Out(12262) = 1 Dly 0.5 '�{�̈ʒu���ߏo�[ON
569 '    Wait M_In(11273) = 1     '�{�̈ʒu���ߏo�[���o
570     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '�{�̈ʒu���ߏo�[���o
571     If MRtn = 1 Then GoTo *CompPositioning
572     M_Out(12263) = 1 Dly 0.5
573     fErrorProcess(11,231,282,0)
574     If M_20# = MNext% Then M_20# = MClear%
575     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
576     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
577     If M_20# = MContinue% Then GoTo *RE_POSITIONING
578     M_Out(12262) = 1 Dly 0.5
579     *CompPositioning
580     '
581 '    Wait M_In(11888) = 1        '�˂����{2��~1��M
582     MRtn = fScrewTighenRoboCheck(11888)    '��~��Ԃ���M����
583     *RE_ERR_REL_2
584     If M_20# = MContinue% Then M_20# = MRtn2
585     If MRtn = 0 Then
586         MRtn2 = 1       'MRtn2������
587         M_Out(12263) = 1 Dly 0.5    '���i�ʒu���߂�����
588         Mov PInitialPosition  '"�C�j�V�����ɖ߂铮��"
589         MRtn2 = frInCheck(11274,1,MSETTIMEOUT05&)
590         If MRtn2 = 0 Then
591             MRtn2 = M_20#                   '�ʒu���ߖߒ[�G���[�Ȃ�M_20#�������ꎞ���
592             M_20# = MClear%                 'M_20#������
593             fErrorProcess(11,234,284,0)     '�ʒu���ߖߒ[�G���[
594             If M_20# = MNext% Then M_20# = MRtn2        'M_20#�ɔ����l����
595                 '�ʒu���߃G���[���������čH���𔲂���ꍇ��~�������s��
596             If M_20# = MNgProcess% Then M_20# = MAbout%
597             Break
598         EndIf
599         Break
600             EndIf
601     If M_20# = MContinue% Then GoTo *RE_ERR_REL_2
602     If MRtn = 0 Then GoTo *ASSY_ERROR_END
603     '
604 '
605 '    Mov PProductOnPltGet_1      '�{�̎󂯎����(�����ʒu�ύX2/11����)
606     '
607     *RE_PLT_GET_1
608     '
609     M_Out(12256) = 0            '�{�̃`���b�N��OFF(�����ʒu�ύX2/11����)
610     M_Out(12257) = 1            '�{�̃`���b�N�JON
611     '
612 '    Wait M_In(11265) = 1        '�{�̃`���b�N�J���o
613     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
614     If MRtn = 1 Then GoTo *CompPltGet1
615     fErrorProcess(11,244,284,0)
616     If M_20# = MNext% Then M_20# = MClear%
617     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
618     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
619     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
620     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
621     *CompPltGet1
622     Mov PProductOnPltGet_1      '�{�̎󂯎����(�����ʒu�ύX2/11����)
623     '
624     Ovrd 25
625 '    Fine 0.05 , P
626     Mvs PProductOnPltGet        '�{�̎󂯎��ʒu
627     Dly 0.1
628     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
629     M_Out(12256) = 1            '�{�̃`���b�N��ON
630 '    Fine 0 , P
631     '
632     M_Out(12263) = 1 Dly 0.5                    '�{�̈ʒu���ߖߒ[ON
633 '    Wait M_In(11274) = 1     '�{�̈ʒu���ߖߒ[���o
634     MRtn = frInCheck(11274,1,MSETTIMEOUT05&)    '�{�̈ʒu���ߖߒ[���o
635     If MRtn = 1 Then GoTo *CompPltGet2
636     M_Out(12256) = 0                            '�{�̃`���b�N��OFF
637     M_Out(12257) = 1                            '�{�̃`���b�N�JON
638     Dly 2.0
639     Mvs PProductOnPltGet_1
640     Mov PProductOnPltGet_2
641     fErrorProcess(11,234,284,0)
642     If M_20# = MNext% Then M_20# = MClear%
643     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
644     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
645     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
646     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
647     Mov PProductOnPltGet_1
648     Mvs PProductOnPltGet
649     M_Out(12257) = 0                            '�{�̃`���b�N�JOFF
650     M_Out(12256) = 1                            '�{�̃`���b�N��ON
651     Dly 2.0
652     *CompPltGet2
653     '
654 '    Wait M_In(11264) = 1        '�{�̌��o
655     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '�{�̌��o
656     If MRtn = 1 Then GoTo *CompPltGet3
657     M_Out(12256) = 0            '�{�̃`���b�N��OFF
658     M_Out(12257) = 1            '�{�̃`���b�N�JON
659     Dly 2.0
660     Mvs PProductOnPltGet_1
661     Mov PProductOnPltGet_2
662     fErrorProcess(11,252,284,0)
663     If M_20# = MNext% Then M_20# = MClear%
664     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
665     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
666     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
667     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
668     Mov PProductOnPltGet_1
669     Mvs PProductOnPltGet
670     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
671     M_Out(12256) = 1            '�{�̃`���b�N��ON
672     Dly 2.0
673     *CompPltGet3
674     '
675 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
676     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
677     If MRtn = 1 Then GoTo *CompPltGet4
678     M_Out(12256) = 0            '�{�̃`���b�N��OFF
679     M_Out(12257) = 1            '�{�̃`���b�N�JON
680     Dly 2.0
681     Mvs PProductOnPltGet_1
682     Mov PProductOnPltGet_2
683     Dly 0.1
684     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
685     M_Out(12256) = 1            '�{�̃`���b�N��ON
686     Dly 3.0
687     fErrorProcess(11,245,284,0)
688     If M_20# = MNext% Then M_20# = MClear%
689     If M_20# = MAbout% Then Mov PInitialPosition    '2022/04/18 �ǉ� �n��
690     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
691     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
692     If M_20# = MContinue% Then GoTo *RE_PLT_GET_1
693     M_Out(12256) = 0            '�{�̃`���b�N��OFF
694     M_Out(12257) = 1            '�{�̃`���b�N�JON
695     Dly 2.0
696     Mov PProductOnPltGet_1
697     Mvs PProductOnPltGet
698     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
699     M_Out(12256) = 1            '�{�̃`���b�N��ON
700     Dly 2.0
701     *CompPltGet4
702     '
703     Dly 0.5                     '�O�̂��߃f�B���C
704     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
705     Mvs PProductOnPltGet_1      '�{�̎󂯎����
706     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
707     Ovrd 100
708     Mov PProductOnPltGet_2      '�{�̎󂯎������_
709     '
710     '���i���˂����{2�ɒu��
711     Ovrd 50                     '�������q�[�g�V���N�O��Ή�100��50(5/12����)
712     Mov PProductOnRoboSet_3     '�o�H
713     '
714     *RE_ROBO_SET_1
715     '
716     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
717     M_Out(12258) = 1            'DVD���J�`���b�N��ON
718 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
719     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
720     If MRtn = 1 Then GoTo *CompRoboSet1
721     fErrorProcess(11,269,284,0)
722     If M_20# = MNext% Then M_20# = MClear%
723     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
724     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
725     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_1
726     *CompRoboSet1
727 '
728     Mov PProductOnRoboSet_2     '�˂����{���i�u�������_
729 '    Mvs PProductOnRoboSet_4     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)
730     Ovrd 10                     '�I�[�o�[���C�h�ύX(22/12/09����)
731     Mvs PProductOnRoboSet_1     '�˂����{���i�u�����
732     Dly 0.1                     '�O�̂���Dly(22/12/09����)
733     Mvs PProductOnRoboSet       '�˂����{���i�u���ʒu
734     M_Out(12866) = 1 Dly 0.3    '�˂����{2����ĊJ(��~1�`��~2)
735 '    Wait M_In(11889) = 1        '�˂����{2��~2��M
736     MScrewRoboNgFlg% = 0
737     MRtn = fScrewTighenRoboCheck(11889)    '��~��Ԃ���M����
738     If MRtn = 0 Then
739         MScrewRoboNgFlg% = 1
740     EndIf
741 '
742     *RE_ROBO_SET_2
743 '
744     M_Out(12256) = 0            '�{�̃`���b�N��OFF
745     M_Out(12257) = 1            '�{�̃`���b�N�JON
746 '    Wait M_In(11265)            '�{�̃`���b�N�J���o
747     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
748     If MRtn = 1 Then GoTo *CompRoboSet2
749     fErrorProcess(11,244,284,0)
750     If M_20# = MNext% Then M_20# = MClear%
751     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
752     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
753     If M_20# = MContinue% Then GoTo *RE_ROBO_SET_2
754     *CompRoboSet2
755     '
756     Mvs PProductOnRoboSet_1     '�˂����{���i�u�����
757     Mvs PProductOnRoboSet_2     '�˂����{���i�u�������_
758 '    Mvs PProductOnRoboSet_4     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)
759     Ovrd 100
760     Mov PProductOnRoboSet_3     '�o�H
761     '
762     If MScrewRoboNgFlg% = 1 Then Mov PInitialPosition
763     If MScrewRoboNgFlg% = 1 Then GoTo *ASSY_ERROR_END
764 '
765 '
766 '
767     '
768     '�w�ʔ����(�R���v���C�A���X���[�h����11/8����)
769     Mov PPlateBackGet_2         '�w�ʔ󂯎������_
770     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~2�`��~3)
771 '    MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����'12/20��M�ʒu�̕ύX(����)
772 '    If MRtn = 0 Then GoTo *ASSY_ERROR_END
773     Mov PPlateBackGet_1         '�w�ʔ󂯎����
774     '
775     *RE_PLATE_GET
776     '
777     Fine 0.05 , P
778     Ovrd 25
779     Mvs PPlateBackGet           '�w�ʔ󂯎��ʒu
780     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
781     M_Out(12256) = 1            '�{�̃`���b�N��ON
782     '
783 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
784     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
785     If MRtn = 1 Then GoTo *CompPlateGet_1
786     M_Out(12256) = 0            '�{�̃`���b�N��OFF
787     M_Out(12257) = 1            '�{�̃`���b�N�JON
788     Mvs PPlateBackGet_1
789     fErrorProcess(11,245,293,0) '284��293�ɕύX6/7����
790     If M_20# = MNext% Then M_20# = MClear%
791     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
792     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
793     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
794     Mvs PPlateBackGet           '�w�ʔ󂯎��ʒu
795     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
796     M_Out(12256) = 1            '�{�̃`���b�N��ON
797     *CompPlateGet_1
798     Fine 0 , P
799     '
800     Ovrd 10
801     Accel 10 , 10               '25,100��10,10(22/12/09����)
802     Dly 0.7                     '�f�B���C���Ԓ��ߒ�(�c���͊m��)
803 '    CmpG 0.7,0.7,,,,,,       'X,Y���Q�C����0.7�ɕύX
804 '    ColChk Off                  '�Փˌ��mOFF
805 '    Cmp Pos , &B11          'X,Y���R���v���C�A���X���[�h�J�n
806     Cnt 1 , 10 , 10
807     Mvs PPlateBackGet_1         '�w�ʔ󂯎����
808 '    Cmp Off                     '�R���v���C�A���X���[�h�I��
809 '    ColChk On                   '�Փˌ��mON
810     Ovrd 25                     '�I�[�o�[���C�h�ύX50��25(22/12/09����)
811     Mov PPlateBackGet_2         '�w�ʔ󂯎������_
812     Ovrd 100
813     Accel 100 , 100
814     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '�w�ʃp�l���`�F�b�N
815     If MRtn = 1 Then GoTo *CompPlateGet_2
816     Cnt 0
817     fErrorProcess(11,267,293,0)                     '284��293�ɕύX6/7����
818     If M_20# = MNext% Then M_20# = MClear%
819     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
820     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
821     If M_20# = MContinue% Then
822         Mov PPlateBackGet_1
823         Dly 0.3
824         M_Out(12256) = 0            '�{�̃`���b�N��OFF
825         M_Out(12257) = 1            '�{�̃`���b�N�JON
826         Dly 2.0
827     EndIf
828     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
829     *CompPlateGet_2    '
830     '�w�ʔ�u��
831 '    Wait M_In(11889) = 1        '�˂����{2��~2��M
832     ColChk Off
833     Mov PPlateBackSet_12        '�w�ʔu�����
834     Cnt 0
835     '
836     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)        '������x�w�ʃp�l���`�F�b�N
837     If MRtn = 1 Then GoTo *CompPlateGet_3
838     fErrorProcess(11,267,293,0)                     '284��293�ɕύX6/7����
839     If M_20# = MNext% Then M_20# = MClear%
840     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
841     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
842     If M_20# = MContinue% Then
843         Mov PPlateBackGet_2
844         Mov PPlateBackGet_1
845         M_Out(12256) = 0            '�{�̃`���b�N��OFF
846         M_Out(12257) = 1            '�{�̃`���b�N�JON
847         Dly 2.0
848     EndIf
849     If M_20# = MContinue% Then GoTo *RE_PLATE_GET
850     *CompPlateGet_3
851     '
852     ' ���i�����v�����M
853     M_Out(12787) = 1
854 '
855     MRtn = fScrewTighenRoboCheck(11890)    '��~��Ԃ���M����'12/20�ʒu�̕ύX(����)
856     If MRtn = 0 Then Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
857     If MRtn = 0 Then GoTo *ASSY_ERROR_END
858 '
859 '    Mov PPlateBackSet_11        '�o�H1
860     Ovrd 50
861 '    Mov PPlateBackSet_10        '�o�H2
862 '    Mov PPlateBackSet_9         '�o�H3
863 '    Mov PPlateBackSet_8         '�o�H4
864 '    Mov PPlateBackSet_7         '�o�H5
865     Mov PPlateBackSet_6         '�o�H6
866     Ovrd 25
867     Mvs PPlateBackSet_5         '�o�H7
868     Mvs PPlateBackSet_4         '�o�H8
869     Mov PPlateBackSet_3         '�o�H9
870     Mov PPlateBackSet_2         '�o�H10
871     Mov PPlateBackSet_1         '�o�H11
872     Mov PPlateBackSet           '�w�ʔu���ʒu
873     *RE_PLATE_SET
874     M_Out(12256) = 0            '�{�̃`���b�N��OFF
875     M_Out(12257) = 1            '�{�̃`���b�N�JON
876     '
877 '    Wait M_In(11265)            '�{�̃`���b�N�J���o
878     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
879     If MRtn = 1 Then GoTo *CompPlateSet
880     fErrorProcess(11,244,284,0)
881     If M_20# = MNext% Then M_20# = MClear%
882     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
883     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
884     If M_20# = MContinue% Then GoTo *RE_PLATE_SET
885     *CompPlateSet
886     '
887 '-----�b�艟��-------------------------------------(22/12/09����)��������
888 *RE_BUCK_PUSH
889     Mvs PPlateBackPush_2
890 '
891     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
892     M_Out(12256) = 1            '�{�̃`���b�N��ON
893 '
894     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
895 '
896     If MRtn = 1 Then GoTo *CompBuckPushSetting  '����Ȃ牟�������
897 '
898     fErrorProcess(11,245,287,0) '�[�Z���T�[NG���G���[�\��
899         If M_20# = MNext% Then M_20# = MClear%              '���ւ������ꂽ��l��������
900         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '��~�������ꂽ��G���[�G���h��
901         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END    'NG�������ꂽ��G���[�G���h��
902         If M_20# = MContinue% Then GoTo *RE_BUCK_PUSH       '���g���C�������ꂽ�������x����
903 '
904 *CompBuckPushSetting
905 '
906     Mvs PPlateBackPush_1
907     Ovrd 10
908     Mvs PPlateBackPush
909     Dly 0.1
910     Ovrd 25
911     Mvs PPlateBackPush_1
912 *RE_CHUCK_OPEN
913     M_20# = MClear%
914     M_Out(12256) = 0            '�{�̃`���b�N��OFF
915     M_Out(12257) = 1            '�{�̃`���b�N�JON
916     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
917     If MRtn = 1 Then GoTo *CompChuckOpenForBackPush
918     fErrorProcess(11,244,284,0)
919         If M_20# = MNext% Then M_20# = MClear%              '���ւ������ꂽ��l��������
920         If M_20# = MAbout% Then GoTo *ASSY_ERROR_END       '��~�������ꂽ��G���[�G���h��
921         If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END    'NG�������ꂽ��G���[�G���h��
922         If M_20# = MContinue% Then GoTo *RE_CHUCK_OPEN       '���g���C�������ꂽ�������x����
923 *CompChuckOpenForBackPush
924 '-----�b�艟��-------------------------------------(22/12/09����)�����܂�
925     ColChk On
926     Mov PPlateBackSet_12        '�w�ʔu�����
927     '
928     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`��~4)
929     Ovrd 100
930     '
931 ''    ' ���i�����v�����M(�����ʒu�ύX2/27����)
932 '    M_Out(12787) = 1
933     '�˂����{���i�N�����v�Œ�҂�
934 '    Wait M_In(11891) = 1        '�˂����{2��~4��M
935     MRtn = fScrewTighenRoboCheck(11891)    '��~��Ԃ���M����
936     If MRtn = 0 Then Mov PInitialPosition    '"�C�j�V�����ɖ߂铮��"
937     If MRtn = 0 Then GoTo *ASSY_ERROR_END
938     '
939     '�u���ʒu�摜����
940 '    Mov PPlateBackCheck_3       '�摜�����ʒu���
941 '    Mov PPlateBackCheck_2       '�ʉߓ_
942     If M_In(11369) = 0 Then GoTo *CompCheck
943     Mvs PPlateBackCheck         '�m�F�ʒu
944     '
945 *RE_CHECK
946     PInspPosition(1) = PPlateBackCheck
947     MInspGroup%(1) = 2
948     MRtn = ISInspectionSingle(PInspPosition,MInspGroup%,1,-1,1)
949     If M_In(11374) = 0 Then MRtn = 1
950     If MRtn = 1 Then GoTo *CompCheck
951     fErrorProcess(11,43,23,0)
952     If M_20# = MNext% Then M_20# = MClear%
953     If M_20# = MAbout% Or M_20# = MNgProcess% Then
954         Mov PPlateBackSet_12
955         Mov PInitialPosition
956     EndIf
957     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
958     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
959     If M_20# = MContinue% Then GoTo *RE_CHECK
960 *CompCheck
961 '
962     Mov PPlateBackSet_12        '�w�ʔu�����
963 ''    ' ���i�����v�����M
964 '    M_Out(12787) = 1
965 '    Mov PPlateBackCheck_2       '�ʉߓ_
966 '    Mov PPlateBackCheck_3       '�摜�����ʒu���
967     '
968     '�˂����{�������ݑ҂�
969     M_Out(12866) = 1 Dly 0.5    '�˂����{2����ĊJ(��~3�`����)
970 '
971     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
972     M_Out(12259) = 1            'DVD���J�`���b�N�JON
973     '
974     'DVD���J�����
975     Mvs PMechaGet_3             '�o�H1
976     Mvs PMechaGet_2             'DVD���J�󂯎������_
977 '    Wait M_In(11272)            '���i�����@Ready
978 '    MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '���i�����@Ready
979 '    If MRtn = 0 Then
980 '        fErrorProcess()         '�G���[����
981 '    EndIf
982 '
983   ' ���i�����v�����M
984     fnAutoScreenComment(513)    '��ԕ\��[���i�����҂�] 2022/04/26 �n��
985 '    M_Out(12787) = 1
986     '    ' ���i���������҂�(�����ύX2/27����)
987 *RE_FEEDER_READY
988 '    Wait M_In(11810) = 1
989 MRtn = frInCheck(11810,1,MSETTIMEOUT05&)   '�����҂�
990 If MRtn = 1 Then GoTo *CompFeederReady
991 '   ' ���i�����v���I��
992 M_Out(12787) = 0
993 fErrorProcess(11,289,290,0)                '284��290�ɕύX6/7����
994 If M_20# = MNext% Then M_20# = MClear%
995 If M_20# = MAbout% Or M_20# = MNgProcess% Then
996     Mov PMechaGet_2
997     Mov PMechaGet_3
998     Mov PMechaGet_4
999     Mov PInitialPosition
1000 EndIf
1001 If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1002 If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1003     ' ���i�����v��
1004 M_Out(12787) = 1
1005 If M_20# = MContinue% Then GoTo *RE_FEEDER_READY
1006 *CompFeederReady
1007 '    ' ���i�����v���I��
1008     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1009     M_Out(12787) = 0
1010 '
1011     Mvs PMechaGet_1             'DVD���J�󂯎����
1012     '
1013     *RE_MECHA_GET_1
1014     '
1015     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1016     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1017     '
1018 '    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1019     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1020     If MRtn = 1 Then GoTo *CompMechaGet1
1021     Mvs PMechaGet_2
1022     Mvs PMechaGet_3
1023     Mov PMechaGet_4
1024     fErrorProcess(11,270,284,0)
1025     If M_20# = MNext% Then M_20# = MClear%
1026     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1027     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1028     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1029     Mov PMechaGet_3
1030     Mvs PMechaGet_2
1031     Mvs PMechaGet_1
1032     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1033     *CompMechaGet1
1034     '
1035     M_Out(12261) = 0            'DVD���J�V�����_�[��OFF
1036     M_Out(12260) = 1            'DVD���J�V�����_�[�oON
1037 '    Wait M_In(11271) = 1        'DVD���J�V�����_�[�o���o
1038     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�o���o
1039     If MRtn = 1 Then GoTo *CompMechaGet2
1040     Mvs PMechaGet_2
1041     Mvs PMechaGet_3
1042     Mov PMechaGet_4
1043     fErrorProcess(11,271,284,0)
1044     If M_20# = MNext% Then M_20# = MClear%
1045     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1046     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1047     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1048     Mov PMechaGet_3
1049     Mvs PMechaGet_2
1050     Mvs PMechaGet_1
1051     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_1
1052     *CompMechaGet2
1053     '
1054     Ovrd 25
1055     Mvs PMechaGet               'DVD���J�󂯎��ʒu
1056     Dly 0.1                     '�ʒu�o���@(22/12/09����)
1057 '
1058     MRtn = 0
1059     MRtn2 = 0
1060     *RE_MECHA_GET_2
1061     If M_20# = MContinue% Then M_20# = MClear%
1062     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1063     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1064     '
1065 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
1066     If MRtn = 1 Then Dly 1.0
1067     If MRtn = 1 Then GoTo *CompMechaGet3
1068     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1069     If M_20# = MNext% Then GoTo *CompMechaGet3
1070     If MRtn = 1 Then GoTo *CompMechaGet3
1071     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1072     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1073     Dly 2.0
1074     Mvs PMechaGet_1
1075     Mvs PMechaGet_2
1076     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1077     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1078     Mvs PMechaGet_3
1079     Mov PMechaGet_4
1080     fErrorProcess(11,269,284,0)
1081     If M_20# = MNext% Then
1082         M_20# = MClear%
1083         MRtn = 1
1084     EndIf
1085     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1086     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1087     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1088     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1089     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1090     Mov PMechaGet_3
1091     Mvs PMechaGet_2
1092     Mvs PMechaGet_1
1093     Mvs PMechaGet
1094     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1095     *CompMechaGet3
1096     M_20# = MClear%
1097     '
1098 '    Wait M_In(11267) = 1        'DVD���J���o
1099     If MRtn2 = 1 Then GoTo *CompMechaGet4
1100     MRtn2 = frInCheck(11267,1,MSETTIMEOUT05&)   'DVD���J���o
1101     If MRtn2 = 1 Then GoTo *CompMechaGet4
1102     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1103     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1104     Dly 2.0
1105     Mvs PMechaGet_1
1106     Mvs PMechaGet_2
1107     M_Out(12259) = 0            'DVD���J�`���b�N�JOfF
1108     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1109     Mvs PMechaGet_3
1110     Mov PMechaGet_4
1111     fErrorProcess(11,273,284,0)
1112     If M_20# = MNext% Then
1113         M_20# = MClear%
1114         MRtn2 = 1
1115     EndIf
1116     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1117     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1118     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1119     M_Out(12258) = 0            'DVD���J�`���b�N��OfF
1120     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1121     Mov PMechaGet_3
1122     Mvs PMechaGet_2
1123     Mvs PMechaGet_1
1124     Mvs PMechaGet
1125     If M_20# = MContinue% Or M_20# = MClear% Then GoTo *RE_MECHA_GET_2
1126     *CompMechaGet4
1127     M_20# = MClear%
1128     Dly 0.5
1129     '
1130     Mvs PMechaGet_1             'DVD���J�󂯎����
1131 '    *RE_MECHA_GET_3
1132     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1133     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1134 '    Wait M_In(11270) = 1        'DVD���J�V�����_�[�ߌ��o
1135     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�ߌ��o
1136 '    If MRtn = 1 Then GoTo *CompMechaGet5       '�����ʒu�ύX2/11����
1137 '    fErrorProcess(11,272,284,0)
1138 '    If M_20# = MNext% Then M_20# = MClear%
1139 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1140 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1141 '    If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1142 '    *CompMechaGet5
1143     '
1144     If MRtn = 1 Then Ovrd 50    '�O�̂��߃I�[�o�[���C�h�ύX100��50(22/12/09����)
1145     Mvs PMechaGet_2             'DVD���J�󂯎������_
1146 '    ' ���i�����v���I��
1147     M_Out(12787) = 0
1148 '    ' ���i�擾�������M(�p���X)
1149     M_Out(12800) = 1 Dly 0.5
1150     Mvs PMechaGet_3             '�o�H1
1151     Mov PMechaGet_4             '�o�H2
1152 '
1153     *RE_MECHA_GET_3
1154     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1155     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1156     If MRtn = 1 Then GoTo *CompMechaGet5
1157     fErrorProcess(11,272,284,0)
1158     If M_20# = MNext% Then M_20# = MClear%
1159     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1160     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1161     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1162     If M_20# = MContinue% Then GoTo *RE_MECHA_GET_3
1163     *CompMechaGet5
1164     '
1165     'DVD���J�����u����֒u��
1166 '    Wait M_In(11920) = 0             'BaseUnit6�����u����t���O�m�F(�����ʒu�ύX2/11����)
1167 '
1168 '   ���u���䂪��]�����m�F
1169     *Loop_CW_CCW_1
1170     If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_1 Else GoTo *Loop_CW_CCW_1
1171     *Next_CW_CCW_1
1172     If M_In(11920) = 0 Then GoTo *OK_FLG_1 Else GoTo *Loop_CW_CCW_1           'BaseUnit6�����u����t���O�m�F
1173     *OK_FLG_1
1174 '
1175     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1176     '
1177     MRtn = 1
1178     'DVD���J�����u����ɒu����Ă��Ȃ����̊m�F(�ǉ���������10/1����)
1179     If M_In(11931) = 1 Then          '��]�����̊m�F(CW����)
1180         If M_In(11928) = 0 Then      'BaseUnit5����DVD���J���u����Ă����ꍇ
1181             M_Out(12912) = 0         '���u����t���O���(��]�\��Ԃɂ��邽��)
1182             Wait M_In(11930) = 1     '���u�����]�҂�
1183             'If M_In(11929) = 0 Then  '��]��ɂ܂�BaseUnit5���ɒu����Ă���ꍇ
1184                 '�G���[����(BaseUnit6���ɂĐ���ȓ��삪����Ă��Ȃ�)
1185             'Endif
1186         EndIf
1187     ElseIf M_In(11930) = 1 Then      '��]�����̊m�F(CCW����)
1188         If M_In(11929) = 0 Then      'BaseUnit5����DVD���J���u����Ă����ꍇ
1189             M_Out(12912) = 0         '���u����t���O���(��]�\��Ԃɂ��邽��)
1190             Wait M_In(11931) = 1     '���u�����]�҂�
1191             MRtn = 0
1192         EndIf
1193     Else
1194         MRtn = 0'�G���[����(���u���䂪����ȓ�������Ă��Ȃ�)
1195     EndIf
1196     If MRtn = 0 Then GoTo *Loop_CW_CCW_1
1197 '
1198 *Loop_CW_CCW_S
1199     fnAutoScreenComment(530)    '��ԕ\��[�H���U�̓���I���҂�] 2022/04/26 �n��
1200 'Ver 0.4 �ǉ� -----------------------
1201 '���H����12912 = 1 ���o�͂�����A�ēx �H��6���̓��쒆�Ď����s��
1202     MRtn = 0
1203     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec �H��6�̃t���OON�Ď�
1204     If MRtn = 1 Then M_Out(12912) = 0                  '���u����t���O��� �H��6�D��̂���12912=0���o��
1205     If MRtn = 1 Then Dly 0.7
1206     If MRtn = 1 Then GoTo *Loop_CW_CCW_S        '�H��6�̓��쒆�t���O��ON���Ă�����ēx���[�v
1207     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1208 'Ver 0.4 �����܂� -------------------
1209 '
1210     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
1211     Mov PMechaSet_3             'DVD���J���u�����_1(�����ʒu�ύX2/27����)
1212 '
1213 *Loop_CW_CCW_2
1214 'Ver 0.4 �ǉ� -----------------------
1215     '���H����12912 = 1 ���o�͂�����A�ēx �H��6���̓��쒆�Ď����s��
1216     MRtn = 0
1217     MRtn = frInCheck(11920,1,MSETTIMEOUT005&)   '500msec �H��6�̃t���OON�Ď�
1218     If MRtn = 1 Then M_Out(12912) = 0                  '���u����t���O��� �H��6�D��̂���12912=0���o��
1219     If MRtn = 1 Then Dly 0.7
1220     If MRtn = 1 Then GoTo *Loop_CW_CCW_2        '�H��6�̓��쒆�t���O��ON���Ă�����ēx���[�v
1221     M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1222 'Ver 0.4 �����܂� -------------------
1223 '
1224     Mov PMechaSet_2             'DVD���J���u�����_2(�����ʒu�ύX2/27����)
1225 '
1226 '    *Loop_CW_CCW_2  '���u����̏�Ԃ�������x�m�F����(�R�����g�A�E�g2/27����)
1227 '    If M_In(11930) = 1 Or M_In(11931) = 1 Then GoTo *Next_CW_CCW_2 Else GoTo *Loop_CW_CCW_2
1228 '    *Next_CW_CCW_2
1229 '    If M_In(11920) = 0 Then GoTo *OK_FLG_2 Else GoTo *Loop_CW_CCW_2           'BaseUnit6�����u����t���O�m�F
1230 '    *OK_FLG_2
1231 '
1232 '    M_Out(12912) = 1                 '���u����t���O����(���u����̏�ԌŒ�̈�)
1233 '
1234     '
1235     *RE_MECHA_SET_1
1236     If M_20# = MContinue% Then M_20# = MClear%
1237     Ovrd 25
1238     M_Out(12261) = 0            'DVD���J�V�����_�[��OFF
1239     M_Out(12260) = 1            'DVD���J�V�����_�[�oON
1240 '    Wait M_In(11271) = 1        'DVD���J�V�����_�[�o���o
1241     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�o���o
1242     If MRtn = 1 Then GoTo *CompMechaSet1
1243     Mov PMechaSet_3
1244     Mov PMechaGet_4
1245     fErrorProcess(11,271,284,0)
1246     If M_20# = MNext% Then M_20# = MClear%
1247     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1248     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1249     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1250     Mov PMechaSet_3
1251     Mov PMechaSet_2
1252     Ovrd 100
1253     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_1
1254     *CompMechaSet1
1255     '
1256     *RE_MECHA_SET_12
1257     Fine 0.05 , P
1258 '    Wait M_In(11920) = 0        'BaseUnit6�����u����t���O�m�F
1259 '    M_Out(12912) = 1            '���u����t���O����
1260     If M_In(11931) = 1 Then     '��]�����̊m�F(CW����)(�ǉ������܂�10/1����)
1261         Mov PMechaSet1_1        'DVD���J���u�����1
1262         Ovrd 10
1263         Mvs PMechaSet1          'DVD���J���u���ʒu1
1264         Dly 0.1
1265         M_Out(12258) = 0        'DVD���J�`���b�N��OFF
1266         M_Out(12259) = 1        'DVD���J�`���b�N�JON
1267 '        Wait M_In(11268) = 1    'DVD���J�`���b�N�J���o
1268         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1269         Mvs PMechaSet1_1        'DVD���J���u�����1
1270     ElseIf M_In(11930) = 1 Then '��]�����̊m�F(CCW����)(�ǉ���������10/1����)
1271         Mov PMechaSet2_1        'DVD���J���u�����
1272         Ovrd 10
1273         Mvs PMechaSet2          'DVD���J���u���ʒu2
1274         Dly 0.1
1275         M_Out(12258) = 0        'DVD���J�`���b�N��OFF
1276         M_Out(12259) = 1        'DVD���J�`���b�N�JON
1277 '        Wait M_In(11268) = 1    'DVD���J�`���b�N�J���o
1278         MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1279         Mvs PMechaSet2_1        'DVD���J���u�����2
1280     'Else
1281         '�G���[��(���u���䂪����Ȉʒu�ɖ���)
1282     EndIf                       '�ǉ������܂�10/1����
1283     Fine 0 , P
1284     '
1285     If MRtn = 1 Then GoTo *CompMechaSet2
1286     fErrorProcess(11,270,284,0)
1287     If M_20# = MNext% Then M_20# = MClear%
1288     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1289     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1290     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_12
1291     *CompMechaSet2
1292     '
1293     Ovrd 100
1294     *RE_MECHA_SET_2
1295     M_Out(12260) = 0            'DVD���J�V�����_�[�oOFF
1296     M_Out(12261) = 1            'DVD���J�V�����_�[��ON
1297 '    Wait M_In(11270) = 1        'DVD���J�V�����_�[�ߌ��o
1298     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   'DVD���J�V�����_�[�ߌ��o
1299     If MRtn = 1 Then GoTo *CompMechaSet3
1300     fErrorProcess(11,272,284,0)
1301     If M_20# = MNext% Then M_20# = MClear%
1302     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1303     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1304     If M_20# = MContinue% Then GoTo *RE_MECHA_SET_2
1305     *CompMechaSet3
1306     '
1307     Mov PMechaSet_2             'DVD���J���u�����_2
1308     M_Out(12912) = 0                  '���u����t���O���(�ǉ�10/1����)
1309     '
1310     '�˂����{2�̐��i�����
1311     Mov PProductOnRoboGet_4     '�o�H3����4��
1312     M_Out(12259) = 0            'DVD���J�`���b�N�JOFF
1313     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1314 '    Wait M_In(11876) = 1        '�˂����{2�����ҋ@����M
1315 MRtn = fScrewTighenRoboCheck(11876)    '��~��Ԃ���M����
1316 If MRtn = 0 Then Mov PInitialPosition   '"�C�j�V�����ɖ߂铮��"
1317 If MRtn = 0 Then GoTo *ASSY_ERROR_END
1318 '
1319     *RE_ROBO_GET_1
1320 '
1321     M_Out(12259) = 0            'DVD���J�`���b�N�JOFF
1322     M_Out(12258) = 1            'DVD���J�`���b�N��ON
1323     If M_20# = MContinue% Then Dly 0.5
1324 '
1325 '    Wait M_In(11269) = 1        'DVD���J�`���b�N���o
1326     MRtn = frInCheck(11269,1,MSETTIMEOUT05&)   'DVD���J�`���b�N���o
1327     If MRtn = 1 Then GoTo *CompRoboGet1
1328     fErrorProcess(11,269,284,0)
1329     If M_20# = MNext% Then M_20# = MClear%
1330     If M_20# = MAbout% Or M_20# = MNgProcess% Then Mov PInitialPosition
1331     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1332     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1333     If M_20# = MContinue% Then GoTo *RE_ROBO_GET_1
1334     *CompRoboGet1
1335     '
1336     Ovrd 50
1337     Mov PProductOnRoboGet_3     '�˂����{���i�������_2����3��
1338     Ovrd 20
1339     Mvs PProductOnRoboGet_2     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����2��
1340     Mvs PProductOnRoboGet_1     '�˂����{���i�����
1341     Ovrd 10
1342     Mvs PProductOnRoboGet       '�˂����{���i���ʒu
1343     Dly 0.3
1344 '
1345     *RE_ROBO_GET_2
1346 '
1347     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1348     M_Out(12256) = 1            '�{�̃`���b�N��ON
1349 '
1350 '    Wait M_In(11266) = 1        '�{�̃`���b�N���o
1351     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�{�̃`���b�N���o
1352     If MRtn = 1 Or M_20# = MNext% Then GoTo *CompRoboGet2
1353     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1354     M_Out(12257) = 1            '�{�̃`���b�N�JON
1355     Dly 2.0
1356     Mvs PProductOnRoboGet_1
1357     Mvs PProductOnRoboGet_2
1358     Mov PProductOnRoboGet_3
1359     Mov PProductOnRoboGet_4
1360     Mov PInitialPosition
1361     M_Out(12257) = 0            '�{�̃`���b�N�JOFF
1362     M_Out(12256) = 1            '�{�̃`���b�N��ON
1363     Dly 1.0
1364     fErrorProcess(11,245,284,0)
1365     If M_20# = MNext% Then MRtn = 1
1366     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1367     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1368     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1369     M_Out(12257) = 1            '�{�̃`���b�N�JON
1370     Dly 2.0
1371     Mov PProductOnRoboGet_4
1372     Mov PProductOnRoboGet_3
1373     Mov PProductOnRoboGet_2
1374     Mvs PProductOnRoboGet_1
1375     Mvs PProductOnRoboGet
1376     If M_20# = MContinue% Or M_20# = MNext% Then GoTo *RE_ROBO_GET_2
1377     *CompRoboGet2
1378     M_20# = MClear%
1379     '
1380     Accel 30 , 100
1381     Dly 0.3
1382     Mvs PProductOnRoboGet_1     '�˂����{���i�����
1383     Ovrd 25
1384     Mvs PProductOnRoboGet_2     '�˂����{���i�������_(9/27�b��R�����g�A�E�g)12/15�R�����g����
1385     Mvs PProductOnRoboGet_3     '�˂����{���i�u�����(���������]���̋����ł͖����ȏꍇ)11/24�ǉ�(����)4����3��
1386     Ovrd 50                     '�I�[�o�[���C�h�ύX(22/12/09����)
1387     Mov PProductOnRoboGet_4     '�o�H3����4��
1388     Accel 50 , 50               '�����x�ύX(22/12/09����)
1389 '
1390     M_Out(12868) = 1 Dly 0.5    '�˂����{2���슮���𑗐M
1391 '    *RE_ROBO_GET_3                                     '�����ʒu�ύX2/11����
1392 '    M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1393 '    M_Out(12259) = 1            'DVD���J�`���b�N�JON
1394 '    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1395 '    MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1396 '    If MRtn = 1 Then GoTo *CompRoboGet3
1397 '    fErrorProcess(11,270,284,0)
1398 '    If M_20# = MNext% Then M_20# = MClear%
1399 '    If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1400 '    If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1401 '    If M_20# = MContinue% Then GoTo *RE_ROBO_GET_3
1402 '    *CompRoboGet3
1403     '
1404     '�p���b�g�֐��i��u��
1405     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1406     Accel 100 , 100             '�����x�ύX(22/12/09����)
1407     Mov PProductOnPltSet_1      '�{�̒u���ʒu���
1408     Ovrd 10
1409     Mvs PProductOnPltSet        '�{�̒u���ʒu
1410     Dly 0.3
1411 '
1412     *RE_PLT_SET
1413 '
1414     M_Out(12256) = 0            '�{�̃`���b�N��OFF
1415     M_Out(12257) = 1            '�{�̃`���b�N�JON
1416 '
1417 '    Wait M_In(11265) = 1        '�{�̃`���b�N�J���o
1418     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
1419     If MRtn = 1 Then GoTo *CompPltSet
1420     fErrorProcess(11,244,284,0)
1421     If M_20# = MNext% Then M_20# = MClear%
1422     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1423         Mvs PProductOnPltSet_1
1424         Mov PProductOnPltSet_2
1425         Mov PInitialPosition
1426     EndIf
1427     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1428     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1429     If M_20# = MContinue% Then GoTo *RE_PLT_SET
1430     *CompPltSet
1431 '
1432     Mvs PProductOnPltSet_1      '�{�̒u���ʒu���
1433     Ovrd 100
1434     Mov PProductOnPltSet_2      '�{�̒u���ʒu�����_
1435 '    Mov PInitialPosition        '�C�j�V�����|�W�V����
1436     MRtn = FnCtlValue2(2)       '�g���n�j�{�P  2022/04/28 �n��
1437     Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
1438     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1439     *RE_FIN_INI
1440     M_Out(12258) = 0            'DVD���J�`���b�N��OFF
1441     M_Out(12259) = 1            'DVD���J�`���b�N�JON
1442 '    Wait M_In(11268) = 1        'DVD���J�`���b�N�J���o
1443     MRtn = frInCheck(11268,1,MSETTIMEOUT05&)   'DVD���J�`���b�N�J���o
1444     If MRtn = 1 Then GoTo *CompFinIni
1445     fErrorProcess(11,270,284,0)         '���g���C��NG���Ƀ��g���C��������
1446     If M_20# = MNext% Then M_20# = MClear%
1447     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1448     If M_20# = MNgProcess% Then GoTo *RE_FIN_INI
1449     If M_20# = MContinue% Then GoTo *RE_FIN_INI
1450     *CompFinIni
1451     '
1452     '�`�P�b�gID��������
1453     M_20# = MAssyOK%
1454     *ASSY_ERROR_END
1455     *AssyEnd
1456     *fnAssyStart_FEndPosi
1457 FEnd
1458 '
1459 '��fnPiasCheck
1460 ''' <summary>
1461 ''' PIAS�`�P�b�g�Ǎ���
1462 ''' </summary>
1463 ''' <returns>   0 : NG
1464 '''             1 : OK(�Ǎ��݊���)
1465 ''' </returns>
1466 ''' <remarks>
1467 ''' Date   : 2021/07/07 : M.Hayakawa
1468 ''' </remarks>'
1469 Function M% fnPiasCheck
1470     fnPiasCheck = 0
1471     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1472     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1473 '
1474 *RETRY_PIAS
1475     M_20# = MClear%
1476     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1477     '
1478     '�yID�`�P�b�g�ǂݍ��݁z
1479     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1480     MInspGroup%(1) = 1              '����G�ԍ�
1481     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1482 '
1483     '�G���[�̏ꍇ
1484     If MRtn <> 1 Then
1485         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1486         If MRtn <> 1 Then
1487             'D720 -> D1300 �R�s�[�v��
1488             M_Out(12565) = 1
1489             Dly 0.5
1490             M_Out(12565) = 0
1491             '�G���[�����L�q
1492             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1493             'GOT KEY���͑҂�
1494             MKeyNumber = fnKEY_WAIT()
1495             '
1496             Select MKeyNumber
1497                 Case MNext%         '���ւ�I�������ꍇ
1498                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1499                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1500                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1501                     Break
1502                 Case MAbout%        '��~��I�������ꍇ
1503                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1504                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1505                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1506                     Break
1507                 Case MNgProcess%    'NG��I�������ꍇ
1508                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1509                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1510                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1511                     Break
1512                 Case MContinue%     '�p����I�������ꍇ
1513                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1514                     M_20# = MContinue%
1515                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1516                     Break
1517             End Select
1518         EndIf
1519     EndIf
1520 '----------D720 -> D1300 �R�s�[�v��----------
1521     M_Out(12565) = 1
1522     Dly 0.5
1523     M_Out(12565) = 0
1524 '----------�ʐM�m�F������----------
1525     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1526     MRtn = 0                ' ������
1527     M_20# = MClear%         ' ������
1528     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1529     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1530     If MRtn <> 1 Then
1531         If M_20# = MContinue% Then
1532             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1533         Else
1534             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1535         EndIf
1536     EndIf
1537 '----------�H�������m�F----------
1538     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1539     MRtn = 0                ' ������
1540     M_20# = MClear%         ' ������
1541     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1542     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1543     If MRtn <> 1 Then
1544         If M_20# = MContinue% Then
1545             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1546         Else
1547             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1548         EndIf
1549     EndIf
1550     '
1551     fnPiasCheck = 1
1552     *fnPiasCheck_End
1553 FEnd
1554 '
1555 '��fnPCComuCheck
1556 ''' <summary>
1557 ''' PC-PLC�ʐM�`�F�b�N
1558 ''' </summary>
1559 ''' <returns>   0 : NG
1560 '''             1 : OK(�Ǎ��݊���)
1561 ''' </returns>
1562 ''' <remarks>
1563 ''' Date   : 2021/07/07 : M.Hayakawa
1564 ''' </remarks>'
1565 Function M% fnPCComuCheck
1566     fnPCComuCheck = 0
1567     MJudge% = 0                                  '������
1568     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1569     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1570     '
1571     For MStaNo = 0 To 5
1572         '
1573         If M_In(MIN_PIAS_ComOK%) = 1 Then
1574             'PC�ʐMOK(M400)
1575             MJudge% = MOK%
1576             MStaNo = 5
1577             Break
1578         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1579             'toRBT_�ʐM�m�Ftime out
1580             MJudge% = MNG%
1581             MCommentD1001 = 15
1582             MCommentD1002 = 21
1583             MStaNo = 5
1584             Break
1585         Else
1586             'toRBT_�ʐM�m�Ftime out
1587             MJudge% = MNG%
1588             MCommentD1001 = 14
1589             MCommentD1002 = 21
1590             Break
1591         EndIf
1592     Next MStaNo
1593     '
1594     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1595     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1596     '
1597     '�G���[���
1598     If MJudge% <> MOK% Then
1599         M_20# = MClear%     '������
1600         '�G���[�����L�q
1601         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1602         'GOT KEY���͑҂�
1603         MKeyNumber = fnKEY_WAIT()
1604         '
1605         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1606             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1607             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1608             Break
1609         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1610             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1611             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1612             Break
1613         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1614             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1615             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1616             Break
1617         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1618             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1619             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1620             Break
1621         EndIf
1622     Else
1623         'OK�̏ꍇ
1624         fnPCComuCheck = 1
1625     EndIf
1626 FEnd
1627 '
1628 '��fnProcessCheck
1629 ''' <summary>
1630 ''' �H�������m�F
1631 ''' </summary>
1632 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1633 '''             -1�F�O�H������NG  -2�F���H����������
1634 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1635 '''             -5�F���������G���[
1636 ''' </returns>
1637 ''' <remarks>
1638 ''' Date   : 2021/07/07 : M.Hayakawa
1639 ''' </remarks>'
1640 Function M% fnProcessCheck
1641     fnProcessCheck = 0
1642     MJudge% = MNG%      '��UNG���������Ƃ���
1643 '----------�H�������m�F----------
1644     MCommentD1001 = 0   '�R�����g������
1645     For MStaNo = 0 To 5
1646         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1647         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1648         '
1649         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1650             MJudge% = MOK%
1651             fnAutoScreenComment(85)     ' AUTO���
1652             MStaNo = 5
1653             Break
1654         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1655             MFlgLoop% = 0
1656             MJudge% = MNG%
1657             MCommentD1001 = 27
1658             MCommentD1002 = 22
1659             fnAutoScreenComment(94)     ' AUTO���
1660             fnProcessCheck = -2         ' NG��-2��Ԃ�
1661             MStaNo = 5
1662             Break
1663         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1664            MJudge% = MNG%
1665             MCommentD1001 = 31
1666             MCommentD1002 = 22
1667             fnAutoScreenComment(83)     ' AUTO���
1668             fnProcessCheck = -3         ' NG��-3��Ԃ�
1669             MStaNo = 5
1670             Break
1671         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1672             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1673             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1674             MJudge% = MNG%
1675             MCommentD1001 = 32
1676             MCommentD1002 = 22
1677             fnAutoScreenComment(84)     ' AUTO���
1678             fnProcessCheck = -1         ' NG��-1��Ԃ�
1679             Dly 1.0
1680             '�H�������m�FOFF
1681             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1682             Dly 1.0
1683            'MStaNo = 5
1684             Break
1685         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1686             MFlgLoop% = 0
1687             MJudge% = MNG%
1688             MCommentD1001 = 29
1689             MCommentD1002 = 22
1690             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1691             fnProcessCheck = -5         ' NG��-5��Ԃ�
1692             MStaNo = 5
1693             Break
1694         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1695             MJudge% = MNG%
1696             If MCommentD1001 = 32 Then
1697                 '�������Ȃ�
1698             Else
1699                 MCommentD1001 = 26
1700             EndIf
1701             MCommentD1002 = 22
1702             fnProcessCheck = -4         ' NG��-4��Ԃ�
1703             MStaNo = 5
1704             Break
1705         Else
1706             MJudge% = MNG%
1707             MCommentD1001 = 28
1708             MCommentD1002 = 22
1709         EndIf
1710     Next MStaNo
1711     '�H�������m�FOFF
1712     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1713     '�ʉߗ���NG �H�������̏ꍇ
1714     If MJudge% = MPass% Then
1715         M_20# = MPass%
1716     EndIf
1717     '
1718     '�G���[���
1719     If MJudge% <> MOK% Then
1720         M_20# = MClear%     '������
1721         '�G���[�����L�q
1722         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1723         'GOT KEY���͑҂�
1724         MKeyNumber = fnKEY_WAIT()
1725         '
1726         Select MKeyNumber
1727             Case MAbout%        '��~��I�������ꍇ
1728                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
1729                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1730                 Break
1731             Case MNext%         '���ւ�I�������ꍇ
1732                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
1733                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1734                 Break
1735             Case MContinue%     '�p����I�������ꍇ
1736                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
1737                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1738                 Break
1739             Case MNgProcess%    'NG��I�������ꍇ
1740                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
1741                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1742                 Break
1743         End Select
1744     Else
1745         fnProcessCheck = 1  ' OK��1��Ԃ�
1746     EndIf
1747 FEnd
1748 '
1749 '��fnPiasWrite
1750 ''' <summary>
1751 ''' Pias �g�����ʏ����ݗv��
1752 ''' </summary>
1753 '''<param name="MFlg%">
1754 '''                 MOK%(1) = �H��������OK��������
1755 '''                 MNG%(0) = �H��������NG��������
1756 '''</param>
1757 '''<returns></returns>
1758 ''' <remarks>
1759 ''' Date   : 2021/07/07 : M.Hayakawa
1760 ''' </remarks>'
1761 Function M% fnPiasWrite(ByVal MFlg%)
1762       fnPiasWrite = 0
1763 *RETRY_PIASWRITE
1764     '
1765     '�g��OK(MOK%)�̏ꍇ�@M306 ON
1766    '�g��NG(MNG%)�̏ꍇ�@M307 ON
1767     If MFlg% = MOK% Then
1768         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
1769     Else
1770         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
1771     EndIf
1772     Dly 0.1                  '�O�̂���
1773     '
1774     'Pias�֏����݊J�n M305 -> ON
1775     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
1776     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
1777     '
1778     MJudge% = MNG%
1779     '
1780     For MStaNo = 0 To 5
1781         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
1782             MJudge% = MOK%
1783             'MRet = fnAutoScreenComment(85)  'AUTO���
1784             MStaNo = 5
1785             Break
1786         '
1787         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
1788             MJudge% = MNG%
1789             'MRet = fnAutoScreenComment(85)  'AUTO���
1790            MCommentD1001 = 34
1791            MCommentD1002 = 25
1792             MStaNo = 5
1793             Break
1794         '
1795         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
1796             MJudge% = MNG%
1797             'MRet = fnAutoScreenComment(85)  'AUTO���
1798            MCommentD1001 = 35
1799            MCommentD1002 = 25
1800             MStaNo = 5
1801             Break
1802         '
1803         ElseIf M_In(11583) = 1 Then                         '�H����������time out
1804             MJudge% = MNG%
1805             'MRet = fnAutoScreenComment(85)  'AUTO���
1806            MCommentD1001 = 36
1807            MCommentD1002 = 25
1808             MStaNo = 5
1809             Break
1810         '
1811         Else
1812             MJudge% = MNG%
1813            MCommentD1001 = 42
1814            MCommentD1002 = 25
1815         '
1816         EndIf
1817         '
1818     Next MStaNo
1819     '
1820     'Pias�֏����݊J�n M305 -> OfF
1821     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
1822     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
1823     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
1824     '
1825     '
1826     '�ʉߗ���NG �H�������̏ꍇ
1827     If MJudge% = MPass% Then
1828         M_20# = MPass%
1829     EndIf
1830     '
1831    M_20# = MClear%     '������
1832     '
1833     '�G���[���
1834     If MJudge% < MOK% Then
1835     '
1836 '�c���Ă���������ł͎g�p���Ȃ����x��
1837 *RETRY_ERR_WRITE
1838         M_20# = MClear%     '������
1839         '�G���[�����L�q
1840         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1841         'GOT KEY���͑҂�
1842         MKeyNumber = fnKEY_WAIT()
1843         '
1844         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1845             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1846            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1847             Break
1848         '
1849         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1850             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1851             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1852         '
1853         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1854             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1855             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1856         '
1857         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1858             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1859            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1860             Break
1861         '
1862         EndIf
1863         '
1864         If M_20# = MClear% Then *RETRY_ERR_WRITE
1865         '
1866     EndIf
1867     '
1868     If M_20# = MContinue% Then *RETRY_PIASWRITE
1869     '
1870     fnPiasWrite = 1
1871     '
1872 FEnd
1873 '
1874 '��fnPCBNumberCheck
1875 ''' <summary>
1876 ''' Pias ��ԍ��ƍ��v��
1877 ''' </summary>
1878 '''<param name="%"></param>
1879 '''<param name="%"></param>
1880 '''<returns></returns>
1881 ''' <remarks>
1882 ''' Date   : 2021/07/07 : M.Hayakawa
1883 ''' </remarks>'
1884 Function M% fnPCBNumberCheck
1885       fnPCBNumberCheck = 0
1886     '
1887 *RETRY_PCBCHECK
1888     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
1889     'Pias�֊�ƍ��J�n M310 -> ON
1890     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
1891     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
1892     '
1893     MJudge% = MNG%
1894     '
1895     For MStaNo = 0 To 5
1896         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
1897             MJudge% = MOK%
1898             fnAutoScreenComment(96)  'AUTO���
1899             MStaNo = 5
1900             Break
1901         '
1902         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
1903             MJudge% = MNG%
1904             fnAutoScreenComment(97)  'AUTO���
1905             MCommentD1001 = 37
1906             MCommentD1002 = 25
1907             MStaNo = 5
1908             Break
1909         '
1910         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
1911             MJudge% = MNG%
1912             fnAutoScreenComment(98)  'AUTO���
1913             MCommentD1001 = 38
1914             MCommentD1002 = 25
1915             MStaNo = 5
1916             Break
1917         '
1918         ElseIf M_In(11580) = 1 Then                         'time out
1919             MJudge% = MNG%
1920             fnAutoScreenComment(99)  'AUTO���
1921             MCommentD1001 = 39
1922             MCommentD1002 = 25
1923             MStaNo = 5
1924             Break
1925         '
1926         Else
1927             MJudge% = MNG%
1928            MCommentD1001 = 41
1929            MCommentD1002 = 25
1930         '
1931         EndIf
1932         '
1933     Next MStaNo
1934     '
1935     'Pias�֊�ƍ��J�n M310 -> OfF
1936     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
1937     '
1938     '
1939     '�ʉߗ���NG �H�������̏ꍇ
1940     If MJudge% = MPass% Then
1941         M_20# = MPass%
1942     EndIf
1943     '
1944    M_20# = MClear%     '������
1945     '
1946     '�G���[���
1947     If MJudge% < MOK% Then
1948     '
1949 '�c���Ă���������ł͎g�p���Ȃ����x��
1950 *RETRY_ERR_PCBNUMBER
1951         M_20# = MClear%     '������
1952         '�G���[�����L�q
1953         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1954         'GOT KEY���͑҂�
1955         MKeyNumber = fnKEY_WAIT()
1956         '
1957         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
1958             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1959             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1960             Break
1961         '
1962         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
1963             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1964             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1965         '
1966         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
1967             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1968             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1969         '
1970         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
1971             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
1972             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1973             Break
1974         '
1975         EndIf
1976         '
1977         If M_20# = MClear% Then *RETRY_ERR_PCBNUMBER
1978         '
1979     EndIf
1980     '
1981     If M_20# = MContinue% Then *RETRY_PCBCHECK
1982 FEnd
1983 '
1984 '��ScrewTight_S2
1985 ''' <summary>
1986 ''' �˂����߂��s��
1987 ''' </summary>
1988 '''<param name="PScrewPos()">
1989 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
1990 '''             PScrewPos(2)    �F�˂����߉��_
1991 '''             PScrewPos(10)   �F�˂����ߏI������
1992 '''</param>
1993 '''<returns>����
1994 '''         0=�ُ�I���A1=����I��
1995 '''</returns>
1996 ''' <remarks>
1997 ''' Date   : 2021/07/07 : M.Hayakawa
1998 ''' </remarks>'
1999 Function M% ScrewTight_S2(ByVal PScrewPosition())   '�l�W���ߌʐݒ�
2000     ScrewTight_S2 = 0
2001     MOKNGFlg = 0
2002     Ovrd 100
2003     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
2004     ' �b��
2005     Ovrd 5
2006     Mvs PScrewPosition(10),-10    ' �p���b�g��˂�����S�@�̏��ֈړ�
2007 '    Ovrd MOvrdA
2008     '�b��}�X�N
2009 '    M_Out(Y62_Driver)=1     ' �o���N�Z�b�e�B���O�@C1
2010 '    Dly 0.1
2011 '    M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
2012 '    'Spd 8.3 '�O�����C�h100  �������C�h60   '���C�h100-40�@100%�FSpd�@15�@'�˂����ߑ��x�ݒ�
2013 '    Spd MSpdA               '�l�W���ߎ�Spd�ʐݒ�
2014     ' �b��ړ��̂�
2015     Mvs PScrewPosition(10)
2016 '    '
2017 '    Dly 0.1
2018 '    Mvs PScrewPos(2) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
2019 '    Wait M_In(11584)=1          '����/�G���[���o
2020 '    Dly 0.1
2021 '    Spd M_NSpd
2022 '    '
2023 '    If M_In(X28_Driver)=1 Then  '�˂��g�[�^���G���[���o��
2024 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
2025 '        Dly 0.1
2026 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
2027 '        Dly 0.1
2028 '        M_Out(Y63_Driver)=0     '�o���N�Z�b�e�B���O�����@C1
2029 '        Dly 0.1
2030 '        M_Out(Y65_Driver)=0     '�v���O���������@F1
2031 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
2032 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
2033 '        MOKNGFlg = -1
2034 '        ScrewTight_S2 = 0
2035 '    Else
2036 '        Wait M_In(X29_Driver)=1 ' ���튮����
2037 '        Dly 0.1
2038 '        M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
2039 '        Dly 0.1
2040 '        M_Out(Y62_Driver)=0     '�o���N�Z�b�e�B���O����
2041 '        Dly 0.1
2042 '        M_Out(Y6A_VV1)=0        '�˂��z���@OFF
2043 '        Dly 0.1
2044 '        Mvs PScrewPos(2),-80    '�p���b�g��˂�����S�@�̏��ֈړ�
2045 '        ScrewTight_S2 = 1
2046 '    EndIf
2047 ' �b��
2048     Ovrd 10
2049     Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
2050     Ovrd 100
2051 FEnd
2052 '
2053 '��ScrewGet_S3
2054 ''' <summary>
2055 ''' �˂������@����˂��𓾂�
2056 ''' </summary>
2057 '''<param name="%"></param>
2058 '''         PScrewPos(1)    �F�˂�������̂˂����
2059 '''         PScrewPos(2)    �F�˂���������_
2060 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2061 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
2062 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
2063 '''<returns>����
2064 '''         0=�ُ�I���A1=����I���A-1=M�l�W�Z���T�[NG�A-2=M�l�W�Z���T�[ON�A-3=�z���G���[
2065 '''</returns>
2066 ''' <remarks>
2067 ''' Date   : 2021/07/07 : M.Hayakawa
2068 ''' </remarks>'
2069 Function M% ScrewGet_S3(ByVal PScrewPosition())
2070     ScrewGet_S3 = 0
2071     MMScrewJudge% = 0
2072     '�˂������평������G���[�`�F�b�N
2073 ' ���b��폜
2074 '    Wait M_In(X34_ScrewReady1)=1 '�˂�������S��Ready�ɂȂ�܂ő҂@���@���b���҂���Ready�ɂȂ�Ȃ���Δ�����v���O�������K�v�H
2075 '    Ovrd 100
2076 '    If M_In(X33_SS2)=0 Then  'M�˂����o�Z���T��OFF�i�̏�j���Ă����ꍇ
2077 '        Ovrd 30
2078 '        Mvs,-80             '���̏ꏊ����80mm���ֈړ�
2079 '        Mov PInitPos19049   '19049�����ʒu�ֈړ�
2080 '        M_Out(Y6A_VV1)=0    '�˂��z�� Off
2081 '        'NG�Ƃ��Ă����̊֐����甲����
2082 '        ScrewGet_S3 = -1
2083 '        MMScrewJudge% = 1
2084 '        MCommentD1001 = 61
2085 '    EndIf
2086 '    If ScrewGet_S3 = 0 Then
2087 '        'S�^�C�g�p�˂������@��M�˂����������Ă��Ȃ����Ď�
2088 '        MMScrewJudge% = 0 'MMScrewJudge������������
2089 '        MRtn = frInCheck(X32_SS1, 0, MSETTIMEOUT01&)
2090 '        If MRtn = 0 Then
2091 '            Ovrd 30
2092 '            Mvs,-80            '���̏ꏊ����50mm���ֈړ�
2093 '            Mov PInitPos19049  '19049�����ʒu�ֈړ�
2094 '            MMScrewJudge% = 2
2095 '            MRtn = All_CLamp_Release()'�S�ẴN�����v�����֕���
2096 '            MCnt% = 2   '2��ݒ�
2097 '            MCommentD1001 = 62
2098 '        EndIf
2099 '        If MMScrewJudge% = 2 Then
2100 '            ScrewGet_S3 = -2
2101 '        EndIf
2102 '    EndIf
2103 '    'M�l�W���肪ON�̏ꍇ NG�Ƃ��Ċ֐��𔲂���
2104 '    If MMScrewJudge% = 2 Then
2105 '        ScrewGet_S3 = -2
2106 '    EndIf
2107     'S�l�W�p�˂����Y��M�l�W�����m�F�p�����܂�
2108     Ovrd 100
2109     Spd M_NSpd
2110     If MMScrewJudge% = 0 Then
2111         ScrewGet_S3 = 0
2112         M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
2113         MScrewCnt% = 0
2114         MFinCnt% = 2
2115 '        For MCnt% = 0 To MFinCnt%
2116             Mov PScrewPosition(2)        ' �˂������@���_
2117             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
2118             Ovrd 80
2119             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2120             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
2121             Mvs PScrewPosition(10), 1.2
2122             M_Out(Y6A_VV1)=1        ' �˂��z���@ON
2123             '�r�b�g��]
2124             M_Out(Y60_Driver)=1
2125             Dly 0.2
2126             '
2127             Ovrd 100
2128             JOvrd M_NJovrd
2129             Spd M_NSpd
2130             '�l�W�z���m�F�ʒu�ړ�
2131             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
2132             Mvs PScrewPosition(10), -15  ' �l�W�z���m�F�ʒu
2133             '�r�b�g��]��~
2134             'M_Out(Y60_Driver)=0
2135             '
2136             '1�b�ԃl�W�z���m�F
2137 ' �ȉ��b��폜
2138 '            MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2139 '            'MRtn = 0'�����G���[
2140 '            '�z���G���[�̏ꍇ
2141 '            '�l�W���˂����Y�ɖ߂�
2142 '            If MRtn = 0 Then
2143 '                Ovrd 30
2144 '                '�r�b�g��]��~
2145 '                M_Out(Y60_Driver)=0
2146 '                '�l�W�����@���
2147 '                Mvs PScrewPos(1)
2148 '                '�X�ɏ��
2149 '                Mov PScrewPos(1), -75
2150 '                '�l�W�̂Ĉʒu
2151 '                Mov PScrewFeedS021
2152 '                '�z��OFF
2153 '                M_Out(Y6A_VV1)=0 '�˂��z���@OFF
2154 '                Dly 0.2
2155 '                '�j��ON
2156 '                M_Out(Y6B_VB1)=1 '�^��j��ON
2157 '                '�r�b�g��]
2158 '                M_Out(Y61_Driver)=1
2159 '                Dly 0.5
2160 '                '
2161 '                Ovrd 100
2162 '                JOvrd M_NJovrd
2163 '                Spd M_NSpd
2164 '                '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
2165 '                Mov PScrewFeedS021, 10
2166 '                Mov PScrewFeedS021
2167 '                Dly 0.1
2168 '                Mov PScrewFeedS021, 10
2169 '                Mov PScrewFeedS021
2170 '                '
2171 '                '�l�W�����҂�
2172 '                '�r�b�g��]��~
2173 '                M_Out(Y61_Driver)=0
2174 '                Dly 0.1
2175 '                '�j��OFF
2176 '                M_Out(Y6B_VB1)=0 '�^��j��OFF
2177 '                '
2178 '                '
2179 '                '�˂��������Ƃ��āA�ړ��X�ɏ��
2180 '                Mov PScrewPos(1), -75
2181 '                Ovrd 100
2182 '                Spd M_NSpd
2183 '                '�l�W�����@���
2184 '                Mvs PScrewPos(1)
2185 '                '
2186 '                ScrewGet_S3 = -3
2187 '                Break
2188 '                '
2189 '            Else
2190 '                MCnt% = MFinCnt%
2191 '                ScrewGet_S3 = 0
2192 '            EndIf
2193 '        Next  MCnt%
2194         '
2195         Ovrd 100
2196         Spd M_NSpd
2197         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2198         M_Out(Y60_Driver)=0     ' �r�b�g��]��~
2199         M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
2200         Mvs PScrewPosition(10), -15  ' �˂��s�b�N�A�b�v�ʒu -15mm
2201         '������x�z���m�F
2202 ' �ȉ��b��폜
2203 '        MRtn = frInCheck(X2B_VS1, 1, MSETTIMEOUT01&)
2204 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
2205 '            MCommentD1001 = 94
2206 '            MCommentD1002 = 95
2207 '            ScrewGet_S3 = -3
2208 '        EndIf
2209 '        If MRtn = 1 Then      '�z��OK�̏ꍇ
2210 '            ScrewGet_S3 = 1
2211 '        EndIf
2212 '        Break
2213     Else
2214         'M�l�W
2215         If MMScrewJudge% = 2 Then
2216             ScrewGet_S3 = -2
2217         EndIf
2218     EndIf
2219 FEnd
2220 '
2221 '��fnKEY_WAIT()
2222 ''' <summary>
2223 ''' GOT����̃L�[���͑҂�
2224 ''' </summary>
2225 '''<returns>1�F��~    2�F����
2226 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
2227 '''         5�FNG
2228 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
2229 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
2230 '''</returns>
2231 ''' <remarks>
2232 ''' Date   : 2021/07/07 : M.Hayakawa
2233 ''' </remarks>'
2234 Function M% fnKEY_WAIT()
2235     fnKEY_WAIT = 0
2236     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
2237     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
2238     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
2239     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2240     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
2241     Dly 0.2
2242     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
2243     MLocalLoopFlg=1
2244     While MLocalLoopFlg=1
2245         If M_In(11345) = 1 Then         '��~   M5345
2246             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
2247             fnKEY_WAIT = 1
2248             MLocalLoopFlg=-1
2249             Break
2250         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
2251             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
2252             fnKEY_WAIT = 2
2253             MLocalLoopFlg=-1
2254             Break
2255         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
2256             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
2257             fnKEY_WAIT = 3
2258             MLocalLoopFlg=-1
2259             Break
2260         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
2261             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
2262             fnKEY_WAIT = 4
2263             MLocalLoopFlg=-1
2264             Break
2265         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
2266             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
2267             fnKEY_WAIT = 5
2268             MLocalLoopFlg=-1
2269             Break
2270             '
2271         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
2272             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
2273             fnKEY_WAIT = MRobotInit1%
2274             MLocalLoopFlg=-1
2275             Break
2276             '
2277         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
2278             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
2279             fnKEY_WAIT = MRobotInit2%
2280             MLocalLoopFlg=-1
2281             Break
2282             '
2283         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
2284             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
2285             fnKEY_WAIT = MRobotInit3%
2286             MLocalLoopFlg=-1
2287             Break
2288             '
2289         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2290             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2291             fnKEY_WAIT = MRobotInit4%
2292             MLocalLoopFlg=-1
2293             Break
2294             '
2295         Else
2296         EndIf
2297     WEnd
2298     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2299     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2300 FEnd
2301 '
2302 '�� fnAUTO_CTL
2303 ''' <summary>
2304 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2305 ''' </summary>
2306 ''' <remarks>
2307 ''' Date   : 2021/07/07 : M.Hayakawa
2308 ''' </remarks>
2309 Function M% fnAUTO_CTL
2310     fnAUTO_CTL = 0
2311     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2312     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2313     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2314     '
2315     If M_Svo=0 Then             '�T�[�{ON�m�F
2316         Servo On
2317     EndIf
2318     Wait M_Svo=1
2319 FEnd
2320 '
2321 '�� fnWindScreenOpen
2322 ''' <summary>
2323 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2324 ''' </summary>
2325 '''<param name="%"></param>
2326 '''<param name="%"></param>
2327 '''<param name="%"></param>
2328 '''<param name="%"></param>
2329 ''' <remarks>
2330 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2331 ''' MWindReSet = 0     ��ʔ�\��
2332 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2333 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2334 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2335 ''' Date   : 2021/07/07 : M.Hayakawa
2336 ''' </remarks>
2337 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2338     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2339         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2340     EndIf
2341     '
2342     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2343         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2344     EndIf
2345     '
2346     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2347        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2348     EndIf
2349     '
2350     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2351     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2352     Dly 0.5
2353     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2354 FEnd
2355 '
2356 '��FnCtlValue2
2357 ''' <summary>
2358 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2359 ''' </summary>
2360 ''' <param name="MCtlNo%"></param>
2361 ''' <remarks>
2362 ''' Date : 2022/04/28 �n��
2363 ''' </remarks>
2364 '''
2365 '''  1�F������       �{�P
2366 '''  2�F�g���n�j��   �{�P
2367 '''  3�F�g���m�f��   �{�P (���g�p)
2368 '''  4�F�z���G���[�� �{�P
2369 ''' 99�F�Ǐ��J�n�M�� OFF
2370 '''
2371 Function M% FnCtlValue2(ByVal MCtlNo%)
2372     FnCtlValue2 = 1
2373     Select MCtlNo%
2374         Case 1        '�������{�P
2375             M_Out(12569) = 0             '�����݊J�n�M��OFF
2376             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2377             MInputQty = M_In16(11600)    '��������M
2378             MInputQty = MInputQty + 1    '�������{�P
2379             M_Out16(12592) = MInputQty   '���������M
2380             M_Out(12569) = 1             '�����݊J�n�M��ON
2381             Break
2382             '
2383         Case 2        '�g���n�j���{�P
2384             M_Out(12569) = 0             '�����݊J�n�M��OFF
2385             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2386             MAssyOkQty = M_In16(11616)   '�g��OK����M
2387             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2388             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2389             M_Out(12569) = 1             '�����݊J�n�M��ON
2390             Break
2391             '
2392         Case 4        '�z���G���[���{�P
2393             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2394             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2395             MSuctionErrQty = M_In16(11648)         '�z���G���[����M
2396             MSuctionErrQty = MSuctionErrQty + 1    '�z���G���[���{�P
2397             M_Out16(12640) = MSuctionErrQty        '�z���G���[�����M
2398             M_Out(12569) = 1                       '�����݊J�n�M��ON
2399             Break
2400             '
2401         Case 99        '�Ǐ��J�n�M��OFF
2402             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2403             M_Out(12569) = 0        '�����݊J�n�M��OFF
2404             Break
2405             '
2406     End Select
2407     Exit Function
2408 FEnd
2409 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2410 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2411 '-------------------------------------------------------------------------------
2412 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2413 '   ����
2414 '       PInspPos()      �F�����ʒu
2415 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2416 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2417 '       MInspCnt%       �F�����ʒu��
2418 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2419 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2420 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2421 '   �߂�l�F����
2422 '       0=�ُ�I���A1=����I��
2423 '
2424 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2425 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2426 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2427 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2428 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2429 '-------------------------------------------------------------------------------
2430     '----- �����ݒ� -----
2431     Cnt 0                                                           '�ړ�����������(�����l=0)
2432     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2433 '    Cnt 1,0.1,0.1
2434     '�ϐ��錾�E������
2435     Def Inte MNum                                                   '�����ԍ�(������1�`)
2436     MNum% = 1                                                       '�����ԍ������l�ݒ�
2437     Def Inte MEndFlg                                                '�����I���t���O
2438     MEndFlg% = 0
2439     '
2440     '����G�ԍ��ݒ�v���E�������s�v��off
2441     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2442     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2443     '�G���[�ԍ��N���A
2444     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2445     M_Out16(MOUT_InspErrNum) = MInspErrNum
2446     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2447     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2448     '
2449     'Insight Ready check?
2450     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2451         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2452         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2453         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2454         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2455         Exit Function
2456     EndIf
2457     '
2458     '�����ʒu���m�F
2459     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2460         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2461         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2462         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2463         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2464         Exit Function
2465     EndIf
2466     '
2467     '
2468     '
2469     '----- ���C������ -----
2470     '�ݒ肳�ꂽ�����ʒu�����̌������s
2471     While( MEndFlg% = 0 )
2472         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2473         MSetGrNumRetryExitFlg = 0
2474         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2475         While( MSetGrNumRetryExitFlg = 0 )
2476         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2477             '
2478             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2479             '
2480             '----- �����O���[�v�ԍ��ݒ� -----
2481             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2482             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2483             '
2484             '�����ʒu�ֈړ��E�ړ������҂�
2485             Mvs PInspPos( MNum% )                                       '�ړ�
2486             Dly 0.05                                                    '�ړ�������Delay
2487             '
2488             '�����O���[�v�ԍ��ݒ�I���m�F
2489             M_Timer(1) = 0
2490             MExitFlg = 0
2491             While( MExitFlg = 0 )
2492                 '����G�ݒ萳��I��?
2493                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
2494                     MExitFlg = 1
2495                 '
2496                 '����G�ݒ�ُ�I��?
2497                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
2498                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2499                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2500                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
2501                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2502                     EndIf
2503                     MExitFlg = 1
2504                 '
2505                 'timeout�`�F�b�N
2506                 ElseIf 1000 < M_Timer(1) Then
2507                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
2508                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
2509                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
2510                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
2511                     EndIf
2512                     MExitFlg = 1
2513                 EndIf
2514             WEnd
2515             '
2516             '����G�ԍ��ݒ�v��off
2517             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
2518             '
2519             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
2520             'NG�Ȃ���Δ�����
2521             If MCurrentStepErr = 0 Then
2522                 MSetGrNumRetryExitFlg = 1
2523             Else
2524                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2525                 If MSetGrNumRetryCnt = 0 Then
2526                     MSetGrNumRetryExitFlg = 1
2527                 Else
2528                     'Retry�ց@���̑O��Delay
2529                     Dly 0.5
2530                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
2531                 EndIf
2532             EndIf
2533             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
2534             '
2535         WEnd
2536         '
2537         '
2538         '
2539         '----- �������s -----
2540         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
2541             If 0 < MInspGrNum%(MNum%) Then                          '��������?
2542                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
2543                 MInspRetryExitFlg = 0
2544                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
2545                 While( MInspRetryExitFlg = 0 )
2546                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
2547                     '
2548                     '���������m�F
2549                     MRetryCnt = MRetryCnt - 1
2550                     M_Timer(1) = 0
2551                     MExitFlg = 0
2552                     While( MExitFlg = 0 )
2553                     '���������҂�
2554                         '����OK�I��?
2555                         If M_In( MIN_IS_InspOK% ) = 1  Then
2556                             MJudgeOKFlg = 1                         '����OK�t���OON
2557                             MExitFlg = 1
2558                         '
2559                         '����NG�I��?
2560                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
2561                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2562                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2563                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
2564                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2565                                 EndIf
2566                             EndIf
2567                             MExitFlg = 1
2568                         '
2569                         '�����ُ�I��(IS timeout)?
2570                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
2571                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2572                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2573                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
2574                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2575                                 EndIf
2576                             EndIf
2577                             MExitFlg = 1
2578                         '
2579                         'timeout�`�F�b�N
2580                         ElseIf 3000 < M_Timer(1) Then
2581                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
2582                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
2583                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
2584                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
2585                                 EndIf
2586                             EndIf
2587                             MExitFlg = 1
2588                         EndIf
2589                     WEnd
2590                     '
2591                     '�����J�n�v��off
2592                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
2593                     '
2594                     'OK�Ȃ甲����
2595                     If MJudgeOKFlg = 1 Then
2596                         MInspRetryExitFlg = 1
2597                     Else
2598                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
2599                         If MRetryCnt = 0 Then
2600                             MInspRetryExitFlg = 1
2601                         Else
2602                             'Retry�ց@���̑O��Delay
2603                             Dly 0.3
2604                         EndIf
2605                     EndIf
2606                     '
2607                 WEnd
2608             EndIf
2609         EndIf
2610         '
2611         '
2612         '
2613         MNum% = MNum% + 1                                           '����Step+1
2614         '�����I���m�F�@�����I���t���O�Z�b�g
2615         If (MInspCnt% < MNum% ) Then
2616             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
2617         EndIf
2618         'NG���������s������
2619         If MInspErrNum <> 0 Then                                    'NG����?
2620             If MNgContinue% <> 1 Then                               'NG���s?
2621                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2622             EndIf
2623         EndIf
2624     WEnd
2625     '
2626     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2627     If 0 < MZAxis% Then
2628         PCurrentPos = P_Curr                                        '���݈ʒu�擾
2629         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
2630         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
2631     EndIf
2632     '
2633     '�߂�l�ݒ�
2634     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
2635         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
2636     Else
2637         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2638         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2639         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2640     EndIf
2641     '
2642     Fine 0,P    'Fine�؂�(22/12/09����)
2643 FEnd
2644 '
2645 ' ��ISInspection
2646 ''' <summary>
2647 ''' Insight�ɂ��摜�����������s
2648 ''' </summary>
2649 '''<param name="PInspPos()">�����ʒu</param>
2650 '''<param name="MInspGrNum%()">�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j</param>
2651 '''             PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2652 '''<param name="MInspCnt%">�����ʒu��</param>
2653 '''<param name="MZAxis%">�I������Z���ޔ����W�i-1:�����j</param>
2654 '''             �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2655 '''<param name="MNgContinue%">=1�Ō����G���[�ENG�������ɑSStep�̌������s��</param>
2656 '''<returns>    ���� 0=�ُ�I���A1=����I��</returns>
2657 '''         MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2658 '''         MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���"
2659 ''' <remarks>
2660 ''' Date   : 2021/07/07 : M.Hayakawa
2661 ''' </remarks>
2662 'Function M% ISInspection( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2663 '    '�摜�g�p�m�F 0<- �摜�m�F�����̏ꍇ
2664 '    If M_In(11369) = 0 Then            'toRBT_�g�p�m�F
2665 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2666 '    EndIf
2667 ''
2668 '    Cnt 0                                                       '�ړ�����������(�����l=0)
2669 '    Fine 0.05,P                                                 '�ʒu���ߊ��������ݒu�@0.05mm
2670 '    MNum% = 1                                                   '�����ԍ������l�ݒ�
2671 '    Def Inte MEndFlg                                            '�����I���t���O
2672 '    MEndFlg% = 0
2673 '    '
2674 '    '�G���[�ԍ��N���A
2675 '    MInspErrNumSub = 0                                          '�������s�G���[�ԍ�sub
2676 '    MInspErrNum = 0                                             '�������s�G���[�ԍ�
2677 '    M_Out16(MOUT_InspErrNum) = MInspErrNum
2678 '    MInspNGStepNum = 0                                          '�������sNGStep�ԍ�
2679 '    M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2680 '    '
2681 '    If M_In(MIN_IS_Ready) = 0 Then                              'Ready off�Ȃ�I��
2682 '        MInspErrNum = 20                                        '�������s�G���[�ԍ� 20 Insight offline
2683 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2684 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2685 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2686 ''
2687 '    EndIf
2688 '   If M_In(MIN_IS_Ready) = 0 Then *ISInspection_End
2689 '    '
2690 '    '�����ʒu���m�F
2691 '    If MInspCnt% < 1 Or 30 < MInspCnt% Then
2692 '        MInspErrNum = 21                                        '�����f�[�^�Ȃ� 21�@����<1
2693 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2694 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2695 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2696 ''
2697 '    EndIf
2698 '   If MInspCnt% < 1 Or 30 < MInspCnt% Then *ISInspection_End
2699 '    '
2700 '    '�ݒ肳�ꂽ�����ʒu�����̌������s
2701 '    While( MEndFlg% = 0 )
2702 '        '�����I���m�F�@�����I���t���O�Z�b�g
2703 '        If (MInspCnt% < MNum% ) Then
2704 '            MEndFlg% = 1                                        '�����I���t���O�Z�b�g
2705 '        EndIf
2706 '        '
2707 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����J�n�@INSPTAST1
2708 '        If MEndFlg% = 0 Then
2709 '            M_01# = MInspGrNum%(MNum%)                          '����G�ԍ����n��
2710 '        EndIf
2711 '        M_02# = MEndFlg%                                        '�����I���t���O���n��
2712 '        M_05# = MNum%                                           '�����ԍ�(������1�`)
2713 '        '�^�X�N�@����G�ݒ�t���O���n��
2714 '        If MEndFlg% = 0 Then
2715 '            If 0 < MInspGrNum%(MNum%) Then
2716 '                M_03# = 1
2717 '            Else
2718 '                M_03# = 0
2719 '            EndIf
2720 '        Else
2721 '            M_03# = 0
2722 '        EndIf
2723 '        '�^�X�N�@�������ʊm�F�t���O���n��
2724 '        If 1 < MNum% Then
2725 '            If 0 < MInspGrNum%(MNum%-1) Then
2726 '                M_04# = 1
2727 '            Else
2728 '                M_04# = 0
2729 '            EndIf
2730 '        Else
2731 '            M_04# = 0
2732 '        EndIf
2733 '        '
2734 '        '�^�X�N�����J�n
2735 '        M_00# = 1                                               'TASK�����J�n
2736 '        '�^�X�N�����J�n�m�F
2737 '        M_Timer(1) = 0
2738 '        MExitFlg = 0
2739 '        While( MExitFlg = 0 )
2740 '            '�����J�n�����m�F
2741 '            If M_00# = 0 And M_10# = 8 Then
2742 '                MExitFlg = 1
2743 '            EndIf
2744 '            'timeout�`�F�b�N
2745 '            If 2000 < M_Timer(1) Then
2746 '                If MNgContinue% = 1 Then                        'NG���s?
2747 '                    MInspErrNumSub = 36                         '�G���[�ԍ��ݒ�36
2748 '                Else
2749 '                    MInspErrNum = 36                            '�G���[�ԍ��ݒ�36
2750 '                EndIf
2751 '                MExitFlg = 1
2752 '            EndIf
2753 '        WEnd
2754 '        '
2755 '        '�����ʒu�ֈړ��E�ړ������҂�
2756 '        If 0 = MInspErrNum Then
2757 '            If MEndFlg% = 0 Then
2758 '                Mvs PInspPos( MNum% )                           '�ړ�
2759 '            EndIf
2760 '        EndIf
2761 '        '
2762 '        '�^�X�N�@����G�ԍ��ݒ�E���������m�F�����I���҂��@INSPTAST1
2763 '        If 0 = MInspErrNum Then
2764 '            M_Timer(1) = 0
2765 '            MExitFlg = 0
2766 '            While( MExitFlg = 0 )
2767 '                '���������҂��i����I���j
2768 '                If M_10# = 1 Then
2769 '                    MExitFlg = 1
2770 '                EndIf
2771 '                '���������҂��i�ُ�I���j
2772 '                If M_10# = 0 Then
2773 '                    If MNgContinue% = 1 Then                    'NG���s?
2774 '                        MInspErrNumSub = M_12#                  '�G���[�ԍ��ݒ�@M12
2775 '                    Else
2776 '                        MInspErrNum = M_12#                     '�G���[�ԍ��ݒ�@M12
2777 '                    EndIf
2778 '                    MExitFlg = 1
2779 '                EndIf
2780 '                'timeout�`�F�b�N
2781 '                If 5000 < M_Timer(1) Then
2782 '                    If MNgContinue% = 1 Then                    'NG���s?
2783 '                        MInspErrNumSub = 31                     '�G���[�ԍ��ݒ�31
2784 '                    Else
2785 '                        MInspErrNum = 31                        '�G���[�ԍ��ݒ�31
2786 '                    EndIf
2787 '                    MExitFlg = 1
2788 '                EndIf
2789 '            WEnd
2790 '        EndIf
2791 '        '
2792 '        '�������ʊm�F
2793 '        If 0 = MInspErrNum Then
2794 '            If 1 < MNum% Then
2795 '                If 0 < MInspGrNum%(MNum%-1) Then                '��������?
2796 '                    If M_11# = 2 Then                           '����NG?
2797 '                        If MNgContinue% = 1 Then                'NG���s?
2798 '                            If MInspNGStepNum = 0 Then          'NG������?
2799 '                                MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2800 '                            EndIf
2801 '                            MInspErrNumSub = 32                 '�G���[�ԍ��ݒ� 32:����NG
2802 '                        Else
2803 ''                            MInspNGStepNum = MNum% - 1          '�������sNGStep�ԍ��ݒ�
2804 '                            MInspNGStepNum = MInspGrNum%(MNum%-1)   '�������sNG�@����G�ԍ��ݒ�
2805 '                            MInspErrNum = 32                    '�G���[�ԍ��ݒ� 32:����NG
2806 '                        EndIf
2807 '                   EndIf
2808 '                EndIf
2809 '            EndIf
2810 '        EndIf
2811 '        '
2812 '        '�G���[�Ȃ猟�����f�I������̂�Loop���甲���邽�ߏI���t���O�Z�b�g
2813 '        If 0 <> MInspErrNum Then
2814 '            MEndFlg% = 1
2815 '        EndIf
2816 '        '
2817 '        '�������s�A�捞�����҂�
2818 '        If 0 = MInspErrNum Then
2819 '            If MEndFlg% = 0 Then
2820 '                If 0 < MInspGrNum%(MNum%) Then                  '��������?
2821 '                    M_Out(MOUT_IS_Insp%) = 1                    '�������s�v��on
2822 '                    '�捞�����m�F
2823 '                    M_Timer(1) = 0
2824 '                    MExitFlg = 0
2825 '                    While( MExitFlg = 0 )
2826 '                        '���������҂�
2827 '                        If M_In( MIN_IS_InspCapDone% ) = 1  Then
2828 '                            MExitFlg = 1
2829 '                        EndIf
2830 '                        'timeout�`�F�b�N
2831 '                        If 2000 < M_Timer(1) Then
2832 '                            If MNgContinue% = 1 Then            'NG���s?
2833 '                                MInspErrNumSub = 33             '�G���[�ԍ��ݒ�33
2834 '                            Else
2835 '                                MInspErrNum = 33                '�G���[�ԍ��ݒ�33
2836 '                            EndIf
2837 '                            MExitFlg = 1
2838 '                        EndIf
2839 '                    WEnd
2840 '                EndIf
2841 '                '
2842 '            EndIf
2843 '        EndIf
2844 '        MNum% = MNum% + 1
2845 '    WEnd
2846 '    '
2847 '    '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2848 '    If 0 < MZAxis% Then
2849 '        PCurrentPos = P_Curr                                    '���݈ʒu�擾
2850 '        PCurrentPos.Z = MZAxis%                                 'Z����ݒ�
2851 '        Mvs PCurrentPos                                         '���݈ʒu���ֈړ�
2852 '    EndIf
2853 '    '
2854 '    'NG���s������
2855 '    If MNgContinue% = 1 Then                                    'NG���s?
2856 '        MInspErrNum = MInspErrNumSub                            '�G���[�ԍ��ݒ�
2857 '    EndIf
2858 '    '
2859 '    '�߂�l�ݒ�
2860 '    If MInspErrNum = 0 Then
2861 '        ISInspection = 1                                        '����I���߂�l�ݒ�
2862 '    Else
2863 '        M_Out16(MOUT_InspErrNum) = MInspErrNum                  '�������s�G���[�ԍ��o��
2864 '        M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum            '�������sNGStep�ԍ��o��
2865 '        ISInspection = 0                                        '�ُ�I���߂�l�ݒ�
2866 '    EndIf
2867 '    '
2868 '*ISInspection_End
2869 'FEnd
2870 '
2871 '��InitialZoneB
2872 ''' <summary>
2873 ''' ����~��̕��A����
2874 ''' 1)���ޔ��@Z������Ɉړ�
2875 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
2876 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
2877 ''' 4)�C�j�V�����|�W�V�����ֈړ�
2878 ''' </summary>
2879 ''' <remarks>
2880 ''' Date : 2022/04/08 : N.Watanabe
2881 ''' </remarks>
2882 Function V fnInitialZoneB()
2883     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/04/26 �n��
2884 '
2885 '�p�����[�^
2886     Ovrd 5
2887 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
2888 '    Cmp Pos, &B100011
2889 '
2890 '���A����J�n
2891 '
2892 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������
2893 *RecoveryChuckOpen
2894     PActive = P_Curr          '���݈ʒu���擾
2895     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
2896 'PProductOnRoboSet(�˂����{���i�u���ʒu)�́A�`���b�N���
2897     If (PActive.X <= PProductOnRoboSet.X + 1.0) And (PActive.X >= PProductOnRoboSet.X -1.0) Then
2898         If (PActive.Y <= PProductOnRoboSet.Y + 1.0) And (PActive.Y >= PProductOnRoboSet.Y -1.0) Then
2899             If (PActive.Z <= PProductOnRoboSet.Z + 1.0) And (PActive.Z >= PProductOnRoboSet.Z -1.0) Then
2900                 MRecoveryChuckOpen = 1
2901             EndIf
2902         EndIf
2903     EndIf
2904 'PProductOnRoboGet(�˂����{���i���ʒu)�́A�`���b�N���
2905     If (PActive.X <= PProductOnRoboGet.X + 1.0) And (PActive.X >= PProductOnRoboGet.X -1.0) Then
2906         If (PActive.Y <= PProductOnRoboGet.Y + 1.0) And (PActive.Y >= PProductOnRoboGet.Y -1.0) Then
2907             If (PActive.Z <= PProductOnRoboGet.Z + 1.0) And (PActive.Z >= PProductOnRoboGet.Z -1.0) Then
2908                 MRecoveryChuckOpen = 1
2909             EndIf
2910         EndIf
2911     EndIf
2912 '
2913 '    If MRecoveryChuckOpen = 1 Then
2914 '        M_Out(12256) = 0        '�{�̃`���b�N��OFF
2915 '        M_Out(12257) = 1        '�{�̃`���b�N�JON
2916 '        M_20# = 0               'KEY���͏�����
2917 '        MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
2918 '        If MRtn = 0 Then
2919 '            fErrorProcess(11,244,284,0)
2920 '            If M_20# = MNext% Then M_20# = MClear%
2921 '            If M_20# = MAbout% Then GoTo *RecoveryEnd
2922 '            If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2923 '            If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2924 '        Else
2925 '            M_Out(12257) = 0        '�{�̃`���b�N�JOFF
2926 '        EndIf
2927 '    EndIf
2928 '
2929     If MRecoveryChuckOpen = 0 Then GoTo *RecoveryChuckOpenEnd
2930     M_Out(12256) = 0                           '�{�̃`���b�N��OFF
2931     M_Out(12257) = 1                           '�{�̃`���b�N�JON
2932 '
2933     M_20# = 0                                  'KEY���͏�����
2934     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�{�̃`���b�N�J���o
2935     If MRtn = 1 Then M_Out(12257) = 0          '�{�̃`���b�N�JOFF
2936     If MRtn = 1 Then GoTo *RecoveryChuckOpenEnd
2937 '
2938     fErrorProcess(11,244,284,0)
2939     If M_20# = MNext% Then M_20# = MClear%
2940     If M_20# = MAbout% Then GoTo *RecoveryEnd
2941     If M_20# = MNgProcess% Then GoTo *RecoveryEnd
2942     If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
2943 '
2944     *RecoveryChuckOpenEnd
2945 '
2946 '�w�ʔ��
2947 'PPlateBackSet�`PPlateBackSet_6�̃G���A�ɂ���Ƃ��́A�{�̃`���b�N�J��
2948 '�EPPlateBackSet_6         '�o�H6
2949 '�EPPlateBackSet_5         '�o�H7
2950 '�EPPlateBackSet_4         '�o�H8
2951 '�EPPlateBackSet_3         '�o�H9
2952 '�EPPlateBackSet_2         '�o�H10
2953 '�EPPlateBackSet_1         '�o�H11
2954 '�EPPlateBackSet           '�w�ʔu���ʒu
2955 '��L�V�_�̂w���W�E�x���W�E�y���W��J6�������LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2956     PActive = P_Curr                    '���݈ʒu���擾
2957     JActive = J_Curr                    '���݈ʒu���擾
2958     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2959     If (PActive.X >= -35) And (PActive.X <= -5) Then
2960         If (PActive.Y >= 340) And (PActive.Y <= 515) Then
2961             If (PActive.Z >= 470) And (PActive.Z <= 560) Then
2962                 If (MJ6 >= 160) And (MJ6 <= 200) Then
2963                     M_Out(12256) = 0            '�{�̃`���b�N��OFF
2964                     M_Out(12257) = 1            '�{�̃`���b�N�JON
2965                 Dly 1.0
2966                 EndIf
2967             EndIf
2968         EndIf
2969     EndIf
2970 '
2971 '
2972 '�������@���ځA���ޔ����o���Ȃ����̑Ώ�
2973 '
2974     Ovrd 1
2975 'PProductOnRoboSet(Get)�`PProductOnRoboSet(Get)_2�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_2��
2976 '�EPProductOnRoboSet
2977 '�EPProductOnRoboSet_1
2978 '�EPProductOnRoboSet_2
2979 '�EPProductOnRoboGet
2980 '�EPProductOnRoboGet_1
2981 '�EPProductOnRoboGet_2
2982 '��L�U�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
2983     PActive = P_Curr                    '���݈ʒu���擾
2984     JActive = J_Curr                    '���݈ʒu���擾
2985     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
2986     If (PActive.X >= -40) And (PActive.X <= 0) Then
2987         If (PActive.Y >= 380) And (PActive.Y <= 420) Then
2988             If (PActive.Z >= 300) And (PActive.Z <= 450) Then
2989                 If (MJ6 >= -20) And (MJ6 <= 20) Then
2990                     Mvs PProductOnRoboSet_1
2991                     Dly 1.0
2992                     Mvs PProductOnRoboSet_2
2993                     Dly 1.0
2994                     Mov PProductOnRoboSet_3
2995                     Dly 1.0
2996                 EndIf
2997             EndIf
2998         EndIf
2999     EndIf
3000 '
3001 'PProductOnRoboSet(Get)_2�`PProductOnRoboSet(Get)_3�̃G���A�ɂ���Ƃ��́APProductOnRoboSet_3��
3002 '�EPProductOnRoboSet_2
3003 '�EPProductOnRoboSet_3
3004 '�EPProductOnRoboGet_2
3005 '�EPProductOnRoboGet_3
3006 '��L�S�_�̂w���W�E�x���W�E�y���W�����LIf���͈̔͂ɓ����Ă��鎖���m�F���鎖
3007     PActive = P_Curr                    '���݈ʒu���擾
3008     JActive = J_Curr                    '���݈ʒu���擾
3009     MJ6 = Deg(JActive.J6)               'J6���̒l���r����ׂɑ��
3010     If (PActive.X >= -40) And (PActive.X <= 0) Then
3011         If (PActive.Y >= 220) And (PActive.Y <= 420) Then
3012             If (PActive.Z >= 400) And (PActive.Z <= 570) Then
3013                 If (MJ6 >= -20) And (MJ6 <= 20) Then
3014                     Mvs PProductOnRoboSet_3
3015                     Dly 1.0
3016                 EndIf
3017             EndIf
3018         EndIf
3019     EndIf
3020 '
3021     Ovrd 5
3022 '
3023 '���ޔ�
3024     PActive = P_Curr
3025     Pmove = PActive
3026     Pmove.Z = 640           '���ޔ�����ꗥ�̍���
3027     If PActive.X > 550 Then
3028         Pmove.Z =550        '�p���b�g��ɘr��L�΂��Ă���Ƃ���640�܂ŏグ���Ȃ��ׁA��O���u
3029     EndIf
3030     If PActive.Z < Pmove.Z Then
3031         Mvs Pmove
3032     EndIf
3033     Dly 1.0
3034 'J1���ȊO��ޔ��|�W�V�����ֈړ�
3035     JActive = J_Curr
3036     Jmove = JTaihi
3037     Jmove.J1 = JActive.J1        'J1���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
3038     Jmove.J6 = JActive.J6        'J6���͌��ݒl���g�p���AJTaihi�̃|�[�Y�����
3039     Mov Jmove
3040     Dly 1.0
3041 'J1���݂̂�ޔ��|�W�V�����ֈړ�
3042     Mov JTaihi
3043     Dly 1.0
3044 '�C�j�V�����|�W�V�����ֈړ�
3045     Mov PInitialPosition
3046     Cmp Off
3047     Ovrd 100
3048 ' �˂����{�������ʒu�ɖ߂����߂ɋ����I�Ɏ����^�]�J�n
3049     If M_In(11856) = 0 Then                 ' ��~���̂�
3050         fnAutoScreenComment(501)            ' ��ԕ\��[�l�W���ߋ@�����^�]�J�n��] 2022/04/25 �n��
3051         M_Out(12834) = 1                    ' �����^�]�J�nON 12834   M6834
3052         MRet = frInCheck(11842, 1, 30000&)  ' �����^�]�J�n��M�҂�    11842   M5842
3053         If MRet = 0 Then
3054         Else
3055             M_Out(12834) = 0    ' �����^�]�J�nOFF 12834   M6834
3056         EndIf
3057     EndIf
3058     M_Out(12262) = 0            '�ʒu���ߏoOFF
3059     M_Out(12263) = 1            '�ʒu���ߖ�ON
3060     fErrorProcess(11,253,281,0)
3061 *RecoveryEnd
3062     Exit Function
3063 FEnd
3064 '
3065 '
3066 '��fnAutoScreenComment
3067 ''' <summary>
3068 ''' ���C����ʂ̓���󋵕\��
3069 ''' �R�����gD1005�̐ݒ�
3070 ''' </summary>
3071 '''<param name="McommentD1005%">�R�����gID</param>
3072 ''' <remarks>
3073 ''' Date   : 2021/07/07 : M.Hayakawa
3074 ''' </remarks>
3075 Function fnAutoScreenComment(ByVal McommentD1005%)
3076     M_Out16(12576) = McommentD1005%
3077 FEnd
3078 '
3079 '��fnRoboPosChk
3080 ''' <summary>
3081 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
3082 ''' </summary>
3083 '''<param name="MINNumber%">���͔ԍ�</param>
3084 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3085 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3086 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
3087 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
3088 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3089 ''' <remarks>
3090 ''' Date   : 2021/07/07 : M.Hayakawa
3091 ''' </remarks>
3092 Function M% fnRoboPosChk
3093     fnRoboPosChk = 0
3094     MRet = fnStepRead()
3095     '�����ʒu�łȂ��Ɣ��f�����ꍇ
3096     '�E�B���h��ʐ؊���
3097     If MRBTOpeGroupNo > 5 Then
3098         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3099         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3100         Dly 0.2
3101         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3102         Dly 1.5
3103         '
3104         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3105         '
3106         MLoopFlg% = 1
3107         While MLoopFlg% = 1
3108             '
3109             '
3110             MKeyNumber% = fnKEY_WAIT()
3111             Select MKeyNumber%
3112                 Case Is = MAbout%       '��~
3113                     M_20# = MAbout%
3114                     MLoopFlg% = -1
3115                     Break
3116                 Case Is = MNext%        '����
3117                     'MLoopFlg% = -1
3118                     Break
3119                 Case Is = MContinue%    '�p��
3120                     M_20# = MContinue%
3121                     MLoopFlg% = -1
3122                     Break
3123                 Default
3124                     Break
3125             End Select
3126         WEnd
3127     EndIf
3128     '
3129     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
3130         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3131         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
3132         Select MRBTOpeGroupNo
3133             Case Is = 5                          '�������Ȃ�
3134                 Break
3135             Case Is = 10                         '�����ʒu�֖߂�
3136                 'Mov PTEST001
3137                 Break
3138             Case Is = 15                         '�����ʒu�֖߂�
3139                 'Mov PTEST002
3140                 Dly 0.5
3141                 'Mov PTEST001
3142                 Dly 0.5
3143                 Break
3144             Default
3145                 Break
3146         End Select
3147         '
3148         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
3149         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
3150         MRBTOpeGroupNo = 5
3151         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
3152         Dly 1.0
3153         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
3154         fnRoboPosChk = 1                        '�����ʒu������s
3155         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3156     EndIf
3157     Exit Function
3158 FEnd
3159 '
3160 '��frInCheck
3161 ''' <summary>
3162 ''' �Z���T�[IN�`�F�b�N
3163 ''' </summary>
3164 '''<param name="MINNumber%">���͔ԍ�</param>
3165 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3166 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3167 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3168 ''' <remarks>
3169 ''' Date   : 2021/07/07 : M.Hayakawa
3170 ''' </remarks>
3171 Function M% frInCheck(MINNumber%, MCMPFLG%, MTimeCnt&)
3172     M_Timer(4) = 0
3173     MloopFlg = 0
3174     While MloopFlg = 0
3175         MCrtTime& = M_Timer(4)
3176         If M_In(MINNumber%) = MCMPFLG% Then
3177             MloopFlg = 1
3178             frInCheck = 1
3179         ElseIf MCrtTime& > MTimeCnt& Then
3180             MloopFlg = 1
3181             frInCheck = 0
3182         EndIf
3183     WEnd
3184 FEnd
3185 '-----------------------------------------------
3186 '
3187 '�˂����ߋ@�ʐM�m�F
3188 '
3189 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3190 'fScrewTcomChk = 0�@�F����I��
3191 '          �@ �@ -1 �F�ُ�I��
3192 '-----------------------------------------------
3193 Function M% fScrewTcomChk
3194 *ReCheckScewTcomChk
3195     fScrewTcomChk = 0
3196     '�ʐM�m�F���M
3197     M_Out(MOUT_ScwT_ComChk%) = MOn%
3198     '�ʐM�m�F��M�ҋ@
3199 '    Wait M_In(MIN_ScwT_comOK%) = MOn%
3200     MRtn = fTimeOutJudge(MIN_ScwT_comOK%,MOn%)
3201     '�ʐM�m�F���M�I��
3202     M_Out(MOUT_ScwT_ComChk%) = MOff%
3203     If MRtn = 0 Then
3204         fScrewTcomChk = -1
3205     EndIf
3206     If MRtn = 2 Then GoTo *ReCheckScewTcomChk
3207  '
3208 FEnd
3209 '
3210 '
3211 '-----------------------------------------------
3212 '
3213 '�˂����ߊJ�n���M
3214 '
3215 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3216 'fScrewTStart = 0�@�F����I��
3217 '           �@�@-1 �F�ُ�I��
3218 '-----------------------------------------------
3219 Function M% fScrewTStart
3220     fScrewTStart = 0
3221     nRet% = 0
3222     '�˂����ߊJ�n�ҋ@����M
3223 '    Wait M_In(MIN_ScwT_STRec%) = MOn%
3224     MRtn = frInCheck(MIN_ScwT_STRec%,MOn%,MSETTIMEOUT05&)
3225     If MRtn = 0 Then nRet% = -1
3226     If MRtn = 0 Then GoTo *ScrewStartERROR      '�J�n�ł��Ȃ������ꍇ�W�����v
3227     Dly 0.1
3228     '�˂����ߊJ�n��M�𑗐M
3229     M_Out(MOUT_ScwT_ST%) = MOn%
3230     Dly 0.5
3231     'Wait M_In(MTEST_KEY%) = MOn%
3232     '�˂����ߊJ�n���M�I��
3233     M_Out(MOUT_ScwT_ST%) = MOff%
3234     '
3235 *ScrewStartERROR
3236     fScrewTStart = nRet%
3237 FEnd
3238 '
3239 '
3240 '
3241 '-----------------------------------------------
3242 '
3243 '�˂����ߊ�����M
3244 '
3245 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3246 'fScewTFinish = 0�@�F����I��
3247 '          �@ �@-1 �F�ُ�I��
3248 '-----------------------------------------------
3249 Function M% fScewTFinish
3250 *ReCheckScewTFinish
3251     fScewTFinish = 0
3252     '�˂����ߊ����ҋ@����M
3253 '    Wait M_In(MIN_ScwT_Fin%) = MOn%
3254     MRtn = fTimeOutJudge(MIN_ScwT_Fin%,MOn%)
3255     If MRtn = 0 Then
3256         fScewTFinish = -1
3257     EndIf
3258     If MRtn = 2 Then GoTo *ReCheckScewTFinish
3259     If MRtn = 0 Then GoTo *ScewTFinish_ErrEnd
3260     Dly 0.1
3261     '�˂����ߊ�����M�𑗐M
3262     M_Out(MOUT_ScwT_FinOK%) = MOn%
3263     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3264     '�˂����ߊJ�n���M�I��
3265     M_Out(MOUT_ScwT_FinOK%) = MOff%
3266     'Wait M_In(MTEST_KEY%) = MOn%
3267     '
3268 *ScewTFinish_ErrEnd
3269 FEnd
3270 '
3271 '
3272 '-----------------------------------------------
3273 '
3274 '����xx��~��M
3275 '
3276 '22/09/29 Wait���^�C���A�E�g�ł���悤�C��(����)
3277 'fScewTCaseStop = 0�@�F����I��
3278 '          �@   �@-1 �F�ُ�I��
3279 '-----------------------------------------------
3280 Function M% fScewTCaseStop(ByVal MCase%())
3281 *ReCheckScewTCaseStop
3282     fScewTCaseStop = 0
3283     '����xx��~����M
3284     Wait M_In(MCase%(1)) = MOn%
3285     MRtn = fTimeOutJudge(MCase%(1),MOn%)
3286     If MRtn = 0 Then
3287         fScewTCaseStop = -1
3288     EndIf
3289     If MRtn = 2 Then GoTo *ReCheckScewTCaseStop
3290     If MRtn = 0 Then GoTo *ScewTCaseStop_ErrEnd
3291     Dly 0.1
3292     '����xx��~��M�𑗐M
3293     M_Out(MCase%(2)) = MOn%
3294     Dly 0.5                          '�Ƃ肠�����ێ�����0.5msec
3295     '�˂����ߊJ�n���M�I��
3296     M_Out(MCase%(2)) = MOff%
3297 *ScewTCaseStop_ErrEnd
3298     '
3299 FEnd
3300 '
3301 '��fScrewTighenRoboCheck
3302 '<summary>
3303 '�˂����{�Ď�
3304 '</summary>
3305 '<param name = "MStopNum%"> ��~�ԍ�</param>
3306 '<returns>���� 0:�˂����{�ُ�I�� 1:OK </returns>
3307 '<make>
3308 '2021/12/2 �����V��
3309 '</make>
3310 Function M% fScrewTighenRoboCheck(ByVal MStopNum%)
3311     fnAutoScreenComment(503)    '��ԕ\��[�˂����{����I���҂�] 2022/04/26 �n��
3312     fScrewTighenRoboCheck = 1
3313     MScrewTighenRoboFlg% = 1    '�t���O�̏�����
3314     MCheck% = 0
3315     While MScrewTighenRoboFlg% = 1
3316         MCheck% = M_In16(11904)
3317         If M_In(MStopNum%) = 1 Then '��~�ʒu�܂ŗ�����
3318             MScrewTighenRoboFlg% = 0 '�֐��𔲂���
3319             fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/04/26 �n��
3320         EndIf
3321         If MCheck% <> 0 Then
3322             fScrewTighenRoboError(MCheck%)
3323             Select M_20#
3324                 Case MAbout%            '��~�������ꂽ�ꍇ
3325                     M_Out(12869) = 1 Dly 1.0
3326                     MScrewTighenRoboFlg% = 0
3327                     fScrewTighenRoboCheck = 0   '�ُ�I��
3328                     Break
3329                 Case MNgProcess%        'NG�������ꂽ�ꍇ
3330                     M_Out(12873) = 1 Dly 1.0
3331                     MScrewTighenRoboFlg% = 0
3332                     fScrewTighenRoboCheck = 0   '�ُ�I��
3333                     Break
3334                 Case MContinue%             '���g���C�������ꂽ�ꍇ
3335                     M_20# = MClear%         'M_20#������
3336                     M_Out(12871) = 1 Dly 1.0
3337                     Break
3338                 Case MNext%                 '���ւ������ꂽ�ꍇ
3339                     M_20# = MClear%         'M_20#������
3340                     M_Out(12874) = 1 Dly 1.0
3341                     Break
3342             End Select
3343             Dly 0.5
3344         EndIf
3345     WEnd
3346 FEnd
3347 '
3348 '��fScrewTighenRoboError
3349 '<summary>
3350 '�˂����{�G���[����
3351 '</summary>
3352 '<param name = "ErrorCode%"> �G���[�ԍ�</param>
3353 '<make>
3354 '2021/12/2 �����V��
3355 '</make>
3356 Function fScrewTighenRoboError(ByVal MErrorCode%)
3357     MErrorScreenCode% = 0
3358     MErrorScreenCode% = MErrorCode% + 300
3359     fErrorProcess(11,MErrorScreenCode%,0,0)
3360 FEnd
3361 '
3362 '��fErrorProcess
3363 '<summary>
3364 '�G���[����
3365 '</summary>
3366 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
3367 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
3368 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
3369 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
3370 '<make>
3371 '2021/11/5 �����V��
3372 '</make>
3373 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3374     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
3375     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
3376     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
3377     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
3378 *RETRY_ERR_PROCESS
3379      M_20# = MClear%     '������
3380 '        '�G���[�����L�q
3381         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3382 '        'GOT KEY���͑҂�
3383         MKeyNumber = fnKEY_WAIT()
3384 '        '
3385         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
3386             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3387             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3388             Break
3389          '
3390         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
3391             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3392             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3393         '
3394         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
3395             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3396             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3397          '
3398         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
3399             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3400             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3401             Break
3402         '
3403         EndIf
3404         '
3405         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3406 FEnd
3407 '
3408 '��fnTorqueCheck
3409 ''' <summary>
3410 ''' �g���N�`�F�b�N����p�̃��C��
3411 ''' </summary>
3412 ''' <remarks>
3413 ''' Date   : 2021/12/21 : H.AJI
3414 ''' </remarks>'
3415 Function M% fnTorqueCheck
3416     '�g���N�`�F�b�N�����M  �����n��~
3417     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3418     '
3419     fnTorqueCheck = 0
3420     Ovrd 20
3421     Mov PInitialPosition              '�����ʒu�ړ�
3422     Ovrd 100
3423     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3424     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3425     Dly 0.2
3426     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3427     '
3428     'M6340  �g���N�`�F�b�N��M
3429     'Dly 5.0
3430     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3431     Dly 1.0
3432     M_Out(12340) = 0
3433     '
3434     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3435     M_Out(12835) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3436    Wait M_In(11843) = 1                         '�˂����{�g���N�`�F�b�N��ʐؑ�
3437     M_Out(12835) = 0                         '�˂����{�g���N�`�F�b�N��ʐؑ֊���
3438     '
3439     '
3440     MLoopFlg = 1
3441     While MLoopFlg = 1
3442         '
3443         Mov PInitialPosition              '�����ʒu�ړ�
3444         '
3445         MKeyNumber = fnKEY_WAIT()
3446         Select MKeyNumber
3447             Case Is = 1           '��~
3448                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3449                 Dly 1.0
3450                 M_Out(12343) = 0
3451                 Ovrd 20
3452                 'Mov PTicketRead_1
3453                 M_Out(12840) = 1          '�g���N�`�F�b�N�I��
3454                 Wait M_In(11859) = 1      '�˂����{����̏I��
3455                 M_Out(12840) = 0          '�g���N�`�F�b�N�I��
3456                 Ovrd 100
3457                 M_20# = 1
3458                 MLoopFlg = -1
3459                 Break
3460             Case Is = 2           '����
3461                 Break
3462             Case Is = 3           '�p��
3463                 Break
3464             Case Is = 4           '�g���N�`�F�b�N�J�n
3465                 M_Out(12342) = 1          '�g���N�`�F�b�N�J�n�v����M M6342
3466                 Dly 1.0
3467                 M_Out(12342) = 0
3468                 If M_In(11862) = 1 Then             '�g���N�`�F�b�J�[�m�F
3469                     fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3470                     MRet = fnScrewMTorque()           '�˂����{�p�g���N�`�F�b�N
3471                 EndIf
3472                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3473                 'MRet = fnMoveTorquePosi()
3474                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3475                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3476                 Break
3477             Default
3478                 Break
3479         End Select
3480     WEnd
3481     '
3482     '�g���N�`�F�b�N����~���M
3483     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3484     '
3485     '���{�b�g�̈ʒu�����ɖ߂�
3486     '
3487     '
3488  FEnd
3489  '
3490 '
3491 '
3492 '---------------------------
3493 '
3494 '    ���C����ʂ̕\���A��\���ݒ�
3495 '         �R�����gD1001, D1002, D1003�̐ݒ�
3496 '           MWindReSet = 0     ��ʔ�\��
3497 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3498 '           MWindErrScr = 10    �G���[��� D1001, D1002
3499 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3500 '
3501 '---------------------------
3502 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3503     fnMainScreenOpen = 0
3504     '
3505    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3506         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3507     EndIf
3508     '
3509     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3510         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3511     EndIf
3512     '
3513     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3514         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3515     EndIf
3516     '
3517     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3518     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3519     Dly 0.5
3520     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3521 FEnd
3522 '
3523 '��Main
3524 ''' <summary>
3525 ''' �g���N�`�F�b�N������
3526 ''' </summary>
3527 ''' <remarks>
3528 ''' Date   : 2021/12/21 : H.AJI
3529 ''' </remarks>'
3530 Function M% fnScrewMTorque
3531     fnScrewMTorque = 0
3532     M_Out(12838) = 1                         '�g���N�`�F�b�N�J�n1
3533     Wait M_In(11857) = 1                     '��M����
3534     M_Out(12838) = 0                         '�g���N�`�F�b�N�J�n1
3535     Dly 2.0
3536 FEnd
3537 '
3538 '��Main
3539 ''' <summary>
3540 ''' �g���N�`�F�b�N������
3541 ''' </summary>
3542 ''' <remarks>
3543 ''' Date   : 2021/12/21 : H.AJI
3544 ''' </remarks>'
3545 Function M% fnMoveTorquePosi
3546      fnMoveTorquePosi = 0
3547      Ovrd 50
3548     'Mov PTorquePosi000 '�g���N�`�F�b�N����ʒu�ֈړ�
3549      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
3550     'Mov PTorquePosi020 '�g���N�`�F�b�N�r�b�g�W���C���g���
3551     '
3552     '
3553      '�ȉ��͈������񂪍쐬�����g���N�`�F�b�N�v���O����
3554     '
3555     Spd M_NSpd
3556 '-------------      �h���C�o�[RST
3557     M_Out(12240)=0     '�h���C�o�[OFF CCW
3558     M_Out(12241)=0     '�h���C�o�[OFF CW
3559     M_Out(12242)=0     '�h���C�o�[���� C1
3560     M_Out(12243)=0     '�h���C�o�[���� C2
3561     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
3562 '---------------------------------------
3563 '---------------------------------------
3564     Fsc Off            '�͊o�Z���T�@Off  STEP1�͕s�v
3565 '--------------------------------------------------------------
3566 '--------------------------------------------------------------
3567 '[P-11]
3568 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
3569     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
3570    'Mov PTorquePosi020, -10                    ' �g���N-1�@�u���ʒu��� 10mm �ֈړ�
3571     Dly 0.1
3572 '-----------------------
3573    'Cnt 0                           'Cnt����-2�@�I��
3574 '-----------------------
3575     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
3576     Dly 0.2
3577 '-----------------------
3578     M_Out(12242)=1                   '�h���C�o�[�Z�b�g C1
3579     Dly 0.1
3580     M_Out(12243)=1                   '�h���C�o�[�Z�b�g C2 (�o���N3)
3581     Dly 0.1
3582     M_Out(12245)=1                   '�v���O����2�Z�b�g F1  M�l�W
3583     Dly 0.1
3584     'M_Out(12241)=1                   '�h���C�o�[ON  CW
3585    M_Out(12241)=0                   '�h���C�o�[OFF  CW
3586     'Dly 0.1
3587 '--------------------------------
3588     Ovrd 40
3589    'Dly 0.1
3590 '--------------------------------  �l�W���ߑ��x�ݒ�
3591     Spd 14                            '���C�h 100-40 100% :Spd 12
3592     Dly 0.1
3593 '--------------------------------
3594 '--------------------------------
3595 '---------------------------------�y�˂����ߓ���z
3596 '
3597     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
3598    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
3599     Dly 0.3                          '�������҂�
3600    M_Out(12241)=1                   '�h���C�o�[ON  CW
3601 '
3602     Wait M_In(11584)=1                '����/�G���[���o
3603     Dly 0.1
3604     Spd M_NSpd
3605    'Ovrd 20
3606     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
3607     Wait M_In(11257)=1                '�l�W����SC
3608 '---------------------------------
3609     Dly 0.1
3610     M_Out(12241)=0                    '�h���C�o�[OFF CW
3611     Dly 0.1
3612     M_Out(12242)=0                    '�h���C�o�[���� C1
3613     Dly 0.1
3614     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
3615     Dly 0.1
3616     M_Out(12245)=0                    '�v���O����2���� F1
3617 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
3618 '
3619     Mvs PTorqueCheck,-60                       '������mov ����ύX
3620     Dly 0.1
3621 '--------------------------------------------------------------
3622    'Ovrd 80
3623 '--------------------------------------------------------------
3624 '---------------------------------------
3625 '---------------------------------------
3626 '---------------------------------------�G���[���E����
3627    *LBL1
3628    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
3629    Mvs ,-100
3630    M_Out(12241)=0     '�h���C�o�[OFF CW
3631    Dly 0.1
3632    M_Out(12242)=0     '�h���C�o�[���� C1
3633    Dly 0.1
3634    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
3635    Dly 0.1
3636    M_Out(12245)=0     '�v���O�������� F1
3637 '---------------------------------------
3638 '---------------------------------------
3639 '-------------
3640    'Mov PInitPos19049
3641    Dly 0.1
3642 '
3643 '
3644 '
3645 '
3646 FEnd
3647 '
3648 '
3649 '----------------------------------------------------------------
3650 'fTimeOutJudge
3651 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3652 '����
3653 'Address% = �Ď��A�h���X�ԍ�
3654 'JudgeFlg% = �ΏۃA�h���X�̐���I�����̒l
3655 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3656 '�߂�l = 0 �G���[
3657 '         1 ����I��
3658 '         2 ���g���C
3659 '::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
3660 '�쐬��
3661 '2022/9/20 ����
3662 '----------------------------------------------------------------
3663 '
3664 Function M% fTimeOutJudge(ByVal MAddress,ByVal MJudgeFlg)
3665     fTimeOutJudge = 0
3666     MJudge% = 1
3667     MRtn = 0
3668     M_20# = MClear%
3669     MRtn = frInCheck(MAddress,MJudgeFlg,15000)
3670 *TimeOutLoop
3671     If MRtn = 1 Then GoTo *TimeOut
3672         fErrorProcess(11,202,203,0)
3673         If M_20# = MNext% Then GoTo *TimeOutLoop
3674         If M_20# = MContinue% Then MJudge% = 2
3675         If M_20# = MAbout% Then GoTo *JUDGE_ERROR_END
3676 *TimeOut
3677     fTimeOutJudge = MJudge%
3678 '
3679 *JUDGE_ERROR_END
3680 FEnd
3681 '��Main
3682 ''' <summary>
3683 ''' �g������p�̃��C��
3684 ''' </summary>
3685 ''' <remarks>
3686 ''' Date   : 2021/07/07 : M.Hayakawa
3687 ''' </remarks>'
3688 Function Main
3689     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3690     '
3691     If M_Svo=0 Then
3692         Servo On
3693     EndIf
3694     Wait M_Svo=1
3695 '�g���X�^�[�g���t�����v���p���XON
3696     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3697 '�p�g���C�g����
3698     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3699     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3700     '
3701     M_20# = 0                                   'KEY���͏�����
3702     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
3703     MRet% = 0
3704 '�����ʒu�̊m�F�ƈړ�
3705 '
3706 '���A����@���s�E�����s����      2022/04/08 �n�� �쐬
3707     PActive = P_Curr                    '���݈ʒu���擾
3708     MRecoveryPass% = 0
3709     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3710         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3711             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3712                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3713             EndIf
3714         EndIf
3715     EndIf
3716     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3717         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3718             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3719                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3720             EndIf
3721         EndIf
3722     EndIf
3723     If MRecoveryPass% = 0 Then
3724        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3725     EndIf
3726 '
3727 '
3728 '    MRet% = fnRoboPosChk()
3729 '    If MRet% = 1 Then                           '�����ʒu�̓�����s�����ꍇ    '2022/04/26 �R�����g�A�E�g �n��
3730 '        fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  '�E�B���h���
3731 '        MKeyNumber% = fnKEY_WAIT()
3732 '        Select MKeyNumber%
3733 '            Case Is = MAbout%       '��~
3734 '                M_20# = MAbout%
3735 '                MLoopFlg% = -1
3736 '                Break
3737 '            Case Is = MNext%        '����
3738 '                'MLoopFlg = -1
3739 '                Break
3740 '            Case Is = MContinue%    '�p��
3741 '                M_20# = MContinue%
3742 '                MLoopFlg% = -1
3743 '                Break
3744 '            Default
3745 '                Break
3746 '        End Select
3747 '    EndIf
3748     '
3749     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3750         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3751 '�g���N�`�F�b�N
3752         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3753             MRet% = fnTorqueCheck()
3754             Break
3755         Else
3756 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
3757 '                MRtn = InspInit()               '�摜��������������
3758 '            EndIf
3759             '
3760            M_20# = MClear%                    '������
3761 '�g���J�n
3762             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3763                 fnAssyStart()
3764             Else
3765                 M_20# = MPass%
3766             EndIf
3767 '�g���I�����t����
3768             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3769             Wait M_In(11572) = 1            '���t�擾����
3770             Dly 0.1
3771             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3772 '���t�^�[���j�b�g�ւ�OUT
3773             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3774             fnAutoScreenComment(89)         'AUTO��� �g����������
3775             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3776 'OK/NG�t���O�o��
3777             If M_20# <= 0 Then
3778                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3779             ElseIf M_20# = MPass% Then
3780                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3781             EndIf
3782 'PIAS�ɑg������������
3783             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3784                 If M_20# = MPass% Then
3785                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3786                 Else
3787                     'KEY���͂�NG�̏ꍇ
3788                     If M_20# = MNgProcess% Then
3789                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3790                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3791                         MRet% = fnPiasWrite(MNG%)
3792                        nAssyNgQty = nAssyNgQty + 1
3793                     EndIf
3794                     '
3795                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3796                     If M_20# = MAssyOK% Then
3797                             '-----------------------
3798                             'D732 -> D2600 �R�s�[�v��
3799                             M_Out(12566) = 1
3800 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3801                             M_Out(12566) = 0
3802                             '
3803                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3804                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3805                             '��ԍ��ƍ�(PP�͖��g�p�j
3806 '                            MRet% = fnPCBNumberCheck()
3807                         Else
3808                             MRet% = 1
3809                         EndIf
3810                         '
3811                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3812                             If M_20# <> MAbout% Then
3813                                 '�H������OK��������
3814                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3815                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3816                                 MRet% = fnPiasWrite(MOK%)
3817                                 nAssyOkQty = 0
3818                                 nAssyOkQty = nAssyOkQty + 1
3819                             Else
3820                                 nAssyOkQty = nAssyOkQty + 1
3821                             EndIf
3822                         EndIf
3823                     EndIf
3824 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3825 '                    MRet% = fnPiasWrite(MOK%)
3826                 EndIf
3827             Else
3828                 nAssyOkQty = nAssyOkQty + 1
3829             EndIf
3830             '
3831             '�g���I�����t��������
3832             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3833             '�������A�g��OK���A�g��NG��������
3834 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3835             '
3836 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3837 '                '�摜�����I������
3838 '                MRtn = InspQuit()
3839 '            EndIf
3840         EndIf
3841         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3842     EndIf
3843 '�p�g���C�g����
3844     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3845     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3846 'GOT�\��
3847     fnAutoScreenComment(93)  'AUTO��� �H������
3848 FEnd
3849 End
3850 '
3851 '���܂��Ȃ��R�����g
3852 '��΍폜�����
3853 '
3854 '
3855 '
3856 '
3857 '
3858 '
3859 '
3860 '
3861 '
PInspPosition(1)=(-74.49,+309.56,+598.75,+180.00,-54.15,+90.00,+0.00,+0.00)(7,1048576)
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
Pmove=(+467.95,+105.02,+640.00,+179.82,+0.33,+179.27,+0.00,+0.00)(7,0)
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
PPlateBackCheck=(-74.49,+309.56,+598.75,+180.00,-54.15,+90.00)(7,1048576)
PPlateBackCheck_2=(-59.41,+338.49,+632.86,+170.41,-66.66,+89.13)(7,1048576)
PPlateBackCheck_3=(-17.86,+286.22,+630.90,-179.69,-0.41,+90.87)(7,1048576)
PPlateBackCheck_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackCheck_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet=(+467.96,+104.98,+401.49,+179.92,+0.33,+179.27)(7,0)
PPlateBackGet_1=(+467.96,+104.98,+430.00,+179.92,+0.33,+179.27)(7,0)
PPlateBackGet_2=(+467.96,+104.98,+560.00,+179.82,+0.33,+179.27)(7,0)
PPlateBackGet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPlateBackPush=(-17.11,+414.76,+540.50,-180.00,+0.00,+88.64)(7,1048576)
PPlateBackPush_1=(-17.11,+414.76,+540.50,-180.00,+0.00,+88.64)(7,1048576)
PPlateBackPush_2=(-17.11,+402.30,+542.92,-179.41,-2.61,+88.95)(7,1048576)
PPlateBackSet=(-17.11,+487.50,+542.92,-179.41,-2.61,+88.95)(7,1048576)
PPlateBackSet_1=(-17.02,+432.03,+532.24,-179.18,-17.77,+89.04)(7,1048576)
PPlateBackSet_10=(-18.77,+363.66,+487.69,-179.91,-41.20,+90.53)(7,1048576)
PPlateBackSet_11=(-18.77,+362.28,+493.38,+179.84,-39.34,+90.58)(7,1048576)
PPlateBackSet_12=(-17.86,+286.22,+630.90,-179.69,-0.41,+90.87)(7,1048576)
PPlateBackSet_2=(-17.65,+400.77,+518.67,-179.12,-27.43,+88.88)(7,1048576)
PPlateBackSet_3=(-18.04,+373.86,+498.91,-179.02,-37.53,+88.69)(7,1048576)
PPlateBackSet_4=(-18.09,+364.27,+491.14,-178.97,-41.13,+88.61)(7,1048576)
PPlateBackSet_5=(-18.67,+359.99,+487.41,-178.97,-41.13,+88.61)(7,1048576)
PPlateBackSet_6=(-18.67,+359.99,+490.17,-178.97,-41.13,+88.61)(7,1048576)
PPlateBackSet_7=(-18.83,+378.77,+503.51,+179.34,-35.63,+90.89)(7,1048576)
PPlateBackSet_8=(-18.81,+374.94,+499.62,+179.31,-36.77,+90.90)(7,1048576)
PPlateBackSet_9=(-18.81,+372.36,+496.87,+179.85,-37.92,+90.58)(7,1048576)
PProductOnPltGet=(+478.76,-97.71,+371.78,+179.90,-0.17,+179.39)(7,0)
PProductOnPltGet_1=(+478.76,-97.71,+410.00,+179.90,-0.17,-179.10)(7,0)
PProductOnPltGet_2=(+478.76,-97.71,+500.00,+179.90,-0.17,-179.10)(7,0)
PProductOnPltGet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet=(+475.92,-96.64,+372.22,+179.83,+0.19,+178.68)(7,0)
PProductOnPltSet_1=(+475.92,-96.64,+410.00,+179.83,+0.19,+178.68)(7,0)
PProductOnPltSet_2=(+475.92,-96.64,+500.00,+179.83,+0.19,+178.68)(7,0)
PProductOnPltSet_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnPltSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboGet=(-19.74,+404.10,+319.10,+85.48,+89.14,+175.44)(6,0)
PProductOnRoboGet_1=(-19.74,+404.10,+395.77,+85.48,+89.14,+175.44)(6,0)
PProductOnRoboGet_2=(-19.74,+396.28,+425.48,-105.62,+89.69,-15.62)(6,0)
PProductOnRoboGet_3=(-19.74,+378.13,+425.48,-105.62,+89.69,-15.62)(6,0)
PProductOnRoboGet_4=(-19.74,+300.00,+550.00,+175.04,+89.99,-94.95)(6,0)
PProductOnRoboGet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboGet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboSet=(-18.93,+403.63,+319.61,+63.21,+89.37,+153.36)(6,0)
PProductOnRoboSet_1=(-18.93,+403.63,+395.81,+63.21,+89.37,+153.36)(6,0)
PProductOnRoboSet_2=(-18.93,+396.29,+419.81,+84.41,+89.14,+174.55)(6,0)
PProductOnRoboSet_3=(-18.91,+236.65,+555.85,-94.47,+89.06,-4.46)(6,0)
PProductOnRoboSet_4=(-18.86,+404.69,+360.00,-102.74,+89.08,-12.36)(6,0)
PProductOnRoboSet_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PProductOnRoboSet_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PPushTilt=(-213.93,+562.96,+465.17,+179.97,-0.02,+4.01)(7,0)
PPushTilt_1=(-213.93,+562.97,+480.78,+179.96,-0.02,+4.01)(7,0)
PPushTilt_2=(-213.93,+562.96,+620.00,+179.97,-0.02,+4.01)(7,0)
PPushTilt_3=(+0.02,+340.00,+610.00,-180.00,-0.01,-91.91)(7,0)
PTicketRead=(+602.00,-150.00,+500.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_1=(+602.00,-150.00,+550.00,+180.00,+0.00,+90.00)(7,0)
PTicketRead_2=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_3=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_4=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_5=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
PTicketRead_6=(+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00,+0.00)(0,0)
JActive=(+12.63,+18.11,+79.30,+0.10,+82.23,+13.34,+0.00,+0.00)
Jmove=(+12.63,-46.87,+111.64,+0.00,+80.58,+13.34,+0.00,+0.00)
JTaihi=(+0.00,-46.87,+111.64,+0.00,+80.58,+0.00)
