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
213 Def Inte MSuctionErrQty     '�z���G���[�� ���Z�ϐ� 2022/04/27 �n��
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
227 Def Inte Y68_VV1            ' �A�[����[�@�l�W�z���o���u
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
269 Y68_VV1%    =  12248    ' �A�[����[�@�l�W�z���o���u '���l12250����12248�֕ύX(8/5����)
270 Y6B_VB1%    =  12250    '�A�[����[�@�z���j��o���u  '���l12251����12250�֕ύX(8/5����)
271 MOUT_VB1%   =  12250    ' �A�[����[�@�l�W�z���j��o���u  '���l12251����12250�֕ύX(8/5����)
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
330 MOUT_ScwT_ComChk% = 12816               '�ʐM�m�F���M
331 MOUT_ScwT_ST% = 12849                   '�˂����ߊJ�n�𑗐M
332 MOUT_ScwT_ReSTOK% = 12850               '�ĊJ�n��M�𑗐M
333 MOUT_ScwT_FinOK% = 12852                '�˂����ߊ�����M�𑗐M
334 MOUT_ScwT_Case1OK% = 12858              '����1��~��M�𑗐M
335 MOUT_ScwT_Case2OK% = 12859              '����2��~��M�𑗐M
336 MOUT_ScwT_Case3OK% = 12860              '����3��~��M�𑗐M
337 MOUT_ScwT_Case4OK% = 12861              '����4��~��M�𑗐M
338 MOUT_ScwT_Case5OK% = 12862              '����5��~��M�𑗐M
339 '
340 MIN_ScwT_comOK% = 11824                 '�˂����ߑ��u����ԐM
341 MIN_ScwT_STRec% = 11857                 '�˂����ߊJ�n����M
342 MIN_ScwT_ReST% = 11858                  '�ĊJ�n����M
343 MIN_ScwT_Fin% = 11860                   '�˂����ߊ�������M
344 MIN_ScwT_Case1% = 11866                 '����1��~�ҋ@����M
345 MIN_ScwT_Case2% = 11867                 '����2��~�ҋ@����M
346 MIN_ScwT_Case3% = 11868                 '����3��~�ҋ@����M
347 MIN_ScwT_Case4% = 11869                 '����4��~�ҋ@����M
348 MIN_ScwT_Case5% = 11870                 '����5��~�ҋ@����M
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
368 Def Inte MStandby              '�ҋ@�ʒu�m�F�t���O
369 Def Inte MRecoveryChuckOpen    '�`���b�N����t���O�i���A����O�j���͂ݑ΍�
370 '�����Ӂ������ʒu��ύX�������ɂ́A�ύX���K�v�I
371 '
372 '===== �y�ʒu�ϐ�(�v�E�e�B�[�`���O�j �����A��`�z =====
373 Function M% fnAssyStart
374     M_25# = 0
375     M_26# = 0
376 ' PIAS�`�P�b�g�Ǎ��ݍH�������m�F
377     M_20# = MClear%                       '������
378 '    If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
379 '        MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
380 '        '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
381 '        '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
382 '        If M_20# = MAbout% Then Mov PInitiaiPosition        ' ���j���[�֖߂�
383 '        If M_20# = MPiasNG% Then Mov PInitiaiPosition       ' NG���H�������ɏ����ݎ��̍H����
384 '        If M_20# = MNgProcess% Then Mov PInitiaiPosition    ' NG���H�������ɏ����ݎ��̍H����
385 '        If M_20# = MPass% Then Mov PInitiaiPosition         ' ����NG, �H������
386 '        If M_20# <> MClear% Then *fnAssyStart_FEndPosi      ' OK�ȊO�͑g���I��
387 '    EndIf
388     ' �l�W���ߋ@�e�X�g�p ----------
389     '    'Mret% = fScewTcomChk()
390     '    '�˂����ߊJ�n
391     '    fScewTStart()
392     '    '
393     '    '���W�ړ�
394     '    '
395     '    '����xx��~
396     '    fScewTCaseStop(MScwT_Case5%)
397     '    '
398     '    '�x�[�X���j�b�gKEY
399     '    Wait M_In(MTEST_KEY%) = MOn%
400     '    '
401     '    '�ĊJ�n
402     '    fScewTReStart()
403     '    '
404     '    '���W�ړ�
405     '    '
406     '    '�˂����ߊ���
407     '    Mret% = fScewTFinish()
408     ' �l�W���߃e�X�g�I��
409     ' PIAS�e�X�g -----------
410     '    MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
411     '    MRet% = fnPiasWrite(MNG%)
412     '    MRet% = fnPCBNumberCheck()
413     ' PIAS�e�X�g�I�� -------
414     '�g�ݗ��ĊJ�n
415     '�v���O�������_
416         '�����ʒu��ݒ�
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
427 '        M_Out(12269) = 0            '�ʒu���ߖ�OFF
428 '        M_Out(12268) = 1            '�ʒu���ߏoON
429 '        Mov PTicketRead
430 '    Else
431 '        Mov PInitialPosition
432 '        M_Out(12269) = 0            '�ʒu���ߖ�OFF
433 '        M_Out(12268) = 1            '�ʒu���ߏoON
434 '        Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
435 '        Mvs PTicketRead             'ID�ǂ݈ʒu
436 '    EndIf
437 '
438 ' 2022/04/12 ���S�����֏����ύX �n��
439 ' PInitialPosition �ݐ� MStandby=2
440 ' PTicketRead_1 �ݐ� MStandby=1
441 '
442     MStandby = 0    '�ҋ@�ʒu�t���O��������
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
457     fnAutoScreenComment(521)        '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
458     If MStandby = 2 Then
459         M_Out(12269) = 0            '�ʒu���ߖ�OFF
460         M_Out(12268) = 1            '�ʒu���ߏoON
461         Mov PTicketRead_1           '�`�P�b�gID�ǂݎ����_
462         Mvs PTicketRead             'ID�ǂ݈ʒu
463     EndIf
464     If MStandby = 1 Then
465         M_Out(12269) = 0            '�ʒu���ߖ�OFF
466         M_Out(12268) = 1            '�ʒu���ߏoON
467         Mvs PTicketRead             'ID�ǂ݈ʒu
468     EndIf
469     If MStandby <> 0 Then GoTo *PositionOK
470     fErrorProcess(11,230,281,0)           '�����ʒu�ɂ��Ȃ����̓G���[�ɂ���
471     If M_20# = MNext% Then GoTo *ASSY_ERROR_END
472     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
473     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
474     If M_20# = MContinue% Then GoTo *ASSY_ERROR_END
475     *PositionOK
476 '
477     Ovrd 100
478     '�n���h�y�ю���ɖ{�̂�������
479     *INITIAL_CHECK
480     If M_In(11264) =0 And M_In(11269) = 0 Then GoTo *CompInitial
481     fErrorProcess(11,253,281,0)
482     If M_20# = MNext% Then M_20# = MClear%
483     If M_20# = MNgProcess% Then M_20# = MAbout%
484     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
485     If M_20# = MContinue% Then GoTo *INITIAL_CHECK
486     *CompInitial
487 '
488     '����������ʒu�ɖ߂�(�ǉ�11/19����)
489     *RE_JIG_INI
490     MRtn = 1
491     MRtn2 = 1
492     If M_In(11276) = 0 Or M_In(11277) = 0 Then  '��]�����Z���^�[�ɗ��Ă��Ȃ����
493         M_Out(12258) = 0        '���i�`���b�N�JOFF
494         M_Out(12259) = 1        '���i�`���b�N��ON
495         MRtn = frInCheck(11271,1,MSETTIMEOUT05&)    '���i�`���b�N���o
496         M_Out(12262) = 0        '��]�X�g�b�p�[�oOFF
497         M_Out(12263) = 1        '��]�X�g�b�p�[��ON
498         MRtn2 = frInCheck(11275,1,MSETTIMEOUT05&)    '��]�X�g�b�p�[�ߒ[���o
499     EndIf
500     If MRtn = 1 And MRtn2 = 1 Then GoTo *CompJigIni1
501     fErrorProcess(11,262,284,0) '0��262�ɕύX6/7����
502     If M_20# = MNext% Then M_20# = MClear%
503     If M_20# = MNgProcess% Then M_20# = MAbout%
504     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
505     If M_20# = MContinue% Then GoTo *RE_JIG_INI
506 *CompJigIni1
507     '
508     If M_In(11278) = 1 Then     'CW�[�ɉ�]���Ă����Ȃ�
509         M_Out(12266) = 0        '�o���u2ON��OFF
510         M_Out(12267) = 1 Dly 0.3       '�o���u2OFF��ON
511         Dly 0.3
512         M_Out(12264) = 0        '�o���u1ON��OFF
513         M_Out(12265) = 1 Dly 0.3       '�o���u1OFF��ON
514     ElseIf M_In(11279) = 1 Then 'CCW�[�ɉ�]���Ă����Ȃ�
515         M_Out(12264) = 0        '�o���u1ON��OFF
516         M_Out(12265) = 1 Dly 0.3       '�o���u1OFF��ON
517         Dly 0.3
518         M_Out(12266) = 0        '�o���u2ON��OFF
519         M_Out(12267) = 1 Dly 0.3       '�o���u2OFF��ON
520     Else
521         M_Out(12264) = 0        '�o���u1ON��OFF
522         M_Out(12265) = 1 Dly 0.3       '�o���u1OFF��ON
523         M_Out(12266) = 0        '�o���u2ON��OFF
524         M_Out(12267) = 1 Dly 0.3       '�o���u2OFF��ON
525     EndIf
526     '
527 '    Wait M_In(11276) = 1 Or M_In(11277) = 1     '��]�Z���^�[���o
528     MRtn = frInCheck(11276,1,MSETTIMEOUT05&)    '��]�Z���^�[���o
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
540     If M_In(11271) = 1 Then     '���i�`���b�N�Ȃ��
541         M_Out(12259) = 0        '���i�`���b�N��OFF
542         M_Out(12258) = 1        '���i�`���b�N�JON
543 '        Wait M_In(11271) = 1    '���i�`���b�N�J���o
544         M_Out(12263) = 0        '��]�X�g�b�p�[��OFF
545         M_Out(12262) = 1        '��]�X�g�b�p�[�oON
546 '        Wait M_In(11274) = 1    '��]�X�g�b�p�[�o�[���o
547     EndIf
548     '
549     M_Out(12261) = 0            '���i�N�����p�[��OFF
550     M_Out(12260) = 1            '���i�N�����p�[�oON
551 '    Wait M_In(11272) = 1        '���i�N�����p�[�o�[���o
552     '
553     M_Out(12256) = 0            '�`���b�N��OFF
554     M_Out(12257) = 1            '�`���b�N�JON
555 '    Wait M_In(11265)            '�`���b�N�J�Z���T�[ON
556 '    MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�`���b�N�J�Z���T�[ON
557 '    If MRtn = 0 Then
558 '        fErrorProcess()         '�G���[����
559 '    EndIf
560     '
561 ''    Mov PInitialPosition
562 MRtn = 1        'MRtn������
563 '�`�P�b�gID��ǂ�
564 *RE_TICKET_READ
565 If M_20# = MContinue% Then M_20# = MClear%
566 'PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
567 'MInspGroup%(1) = 1              '����G�ԍ�
568 'MRtn = ISInspection(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
569 M_20# = MClear%                       '������
570 If M_In(MIN_PIAS_Use%) = 1 Then     'PIAS_ON���̂ݎ��s
571     MRtn = fnPiasCheck()            'PIAS�`�P�b�g��Ǎ��݁A�m�F
572     '�ʐM�m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
573     '�H�������m�F�O���ϐ�����iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
574 EndIf
575 If MRtn = 1 Then GoTo *CompRead
576 'fErrorProcess(11,214,251,0)
577 'If M_20# = MNext% Then M_20# = MClear%
578 'If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
579 'If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
580 If M_20# = MContinue% Then GoTo *RE_TICKET_READ
581 If M_20# = MNext% Then M_20# = MPass%
582 Mvs PTicketRead_1                         '22/04/12 �ǉ� �n��
583 GoTo *ASSY_ERROR_END
584 *CompRead
585     fnAutoScreenComment(521)        '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
586     '
587     '�p���b�g���琻�i�����
588     M_Out(12269) = 0            '�ʒu���ߖ�OFF
589     M_Out(12268) = 1            '�ʒu���ߏoON
590     Mov PProductOnPltGet_2      '���i�����_
591     '
592     *RE_PLT_GET
593     '
594     M_Out(12269) = 0            '�ʒu���ߖ�OFF
595     M_Out(12268) = 1            '�ʒu���ߏoON
596     M_Out(12256) = 0            '�`���b�N��OFF
597     M_Out(12257) = 1            '�`���b�N�JON
598 '
599 '    Wait M_In(11265)            '�`���b�N�J�Z���T�[ON
600     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�`���b�N�J�Z���T�[ON
601     If MRtn = 1 Then GoTo *CompPltGet1
602     fErrorProcess(11,244,284,0)
603     If M_20# = MNext% Then M_20# = MClear%
604     If M_20# = MAbout% Or M_20# = MNgProcess% Then
605         Mov PInitialPosition    '�ޔ����[�g
606         Break
607     EndIf
608     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
609     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
610     If M_20# = MContinue% Then GoTo *RE_PLT_GET
611     *CompPltGet1
612 '
613 '    Mov PProductOnPltGet_1      '���i���(�ʒu�ύX1/14����)
614 '
615 '    Wait M_In(11262) = 1        '�ʒu���ߏo�[�Z���T�[ON
616     MRtn = frInCheck(11262,1,MSETTIMEOUT05&)   '�ʒu���ߏo�[�Z���T�[ON
617     If MRtn = 1 Then GoTo *CompPltGet2
618     fErrorProcess(11,231,282,0)
619     If M_20# = MNext% Then M_20# = MClear%
620     If M_20# = MAbout% Or M_20# = MNgProcess% Then
621         M_Out(12268) = 0            '�ʒu����
622         M_Out(12269) = 1            '�ʒu���ߖ�ON
623         Mov PProductOnPltGet_2
624         Mov PInitialPosition'�ޔ����[�g
625         Break
626     EndIf
627     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
628     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
629     If M_20# = MContinue% Then GoTo *RE_PLT_GET
630     *CompPltGet2
631 '
632     Mov PProductOnPltGet_1      '���i���
633     M_Out(12268) = 0            '�ʒu���ߏoOFF
634     M_Out(12269) = 1            '�ʒu���ߖ�ON
635 '    Wait M_In(11263) = 1        '�ʒu���ߖߒ[�Z���T�[ON
636     MRtn = frInCheck(11263,1,MSETTIMEOUT05&)   '�ʒu���ߖ߂�[�Z���T�[
637     If MRtn = 1 Then GoTo *CompPltGet3
638     fErrorProcess(11,234,284,0)
639     If M_20# = MNext% Then M_20# = MClear%
640     If M_20# = MAbout% Or M_20# = MNgProcess% Then
641         Mov PProductOnPltGet_2      '�ޔ����[�g
642         Mov PInitialPosition
643         Break
644     EndIf
645     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
646     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
647     If M_20# = MContinue% Then GoTo *RE_PLT_GET
648     *CompPltGet3
649 '
650     Ovrd 30
651     Mvs PProductOnPltGet        '���i�����ʒu
652     M_Out(12257) = 0            '�`���b�N�JOFF
653     M_Out(12256) = 1            '�`���b�N��ON
654 '
655 '    Wait M_In(11266) = 1        '�`���b�N�Z���T�[ON
656     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '�`���b�N�Z���T�[ON
657     If MRtn = 1 Then GoTo *CompPltGet4
658     M_Out(12256) = 0        '�ޔ����[�g
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
675     M_Out(12257) = 0            '�`���b�N�JOFF
676     M_Out(12256) = 1            '�`���b�N��ON
677     If M_20# = MContinue% Then GoTo *RE_PLT_GET
678     Mvs PProductOnPltGet
679     Dly 0.1
680     M_Out(12257) = 0            '�`���b�N�JOFF
681     M_Out(12256) = 1            '�`���b�N��ON
682     Dly 2.0
683     *CompPltGet4
684 '
685 '    Wait M_In(11264) = 1        '���i���o�Z���T�[ON
686     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '���i���o�Z���T�[�Z���T�[ON
687     If MRtn = 1 Then GoTo *CompPltGet5
688     fErrorProcess(11,252,284,0)
689     If M_20# = MNext% Then M_20# = MClear%
690     If M_20# = MAbout% Or M_20# = MNgProcess% Then
691         M_Out(12256) = 0        '�ޔ����[�g
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
703     MRtn = FnCtlValue2(1)       '�������{�P  2022/04/28 �n��
704     Mvs PProductOnPltGet_1      '���i���
705     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
706     Ovrd 60
707     Mov PProductOnPltGet_2      '���i�����_
708     '
709     '���i������ɒu��(�_�̒ǉ�,�܂�����ɂ��ϐ����ύX(9/2����))
710     Fine 1.0 , P
711     Mov PProductOnJigSet_4      '�p���b�g�����_(����3����4(9/2����))
712     Fine 0 , P
713     Ovrd 100
714     Mov PProductOnJigSet_3      '�p���b�g-����ԓ_(����2����3(9/2����))
715     Mov PProductOnJigSet_2      '������_(�_�̒ǉ�(9/2����))
716     '
717     *RE_JIG_SET_1
718     '
719     M_Out(12259) = 0            '����i�`���b�N��OFF
720     M_Out(12258) = 1            '����i�`���b�N�JON
721     M_Out(12261)=0              '����i�N�����p�[�����[OFF
722     M_Out(12260)=1              '����i�N�����p�[�o�[ON
723 '
724 '    Wait M_In(11270) = 1        '����i�`���b�N�J�Z���T�[ON
725     MRtn = frInCheck(11270,1,MSETTIMEOUT05&)   '����i�`���b�N�J�Z���T�[ON
726     If MRtn = 1 Then GoTo *CompJIGSet1
727     fErrorProcess(11,259,284,0)
728     If M_20# = MNext% Then M_20# = MClear%
729     If M_20# = MAbout% Or M_20# = MNgProcess% Then
730         Mvs PProductOnJigSet_2      '�ޔ����[�g
731         Mov PProductOnJigSet_3
732         Mov PProductOnJigSet_4
733         Mov PProductOnPltSet_2
734         Mov PProductOnPltSet_1
735         Ovrd 25
736         Mvs PProductOnPltSet
737         Dly 0.3
738         M_Out(12256)=0              '�`���b�N��OFF
739         M_Out(12257)=1              '�`���b�N�JON
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
752 '    Wait M_In(11272) = 1        '�n���h-���i�N�����p�[�J���[�Z���T�[ON
753     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '�n���h-���i�N�����p�[�J���[�Z���T�[ON
754     If MRtn = 1 Then GoTo *CompJIGSet2
755     fErrorProcess(11,257,284,0)
756     If M_20# = MNext% Then M_20# = MClear%
757     If M_20# = MAbout% Or M_20# = MNgProcess% Then
758         Mvs PProductOnJigSet_2      '�ޔ����[�g
759         Mov PProductOnJigSet_3
760         Mov PProductOnJigSet_4
761         Mov PProductOnPltSet_2
762         Mov PProductOnPltSet_1
763         Ovrd 25
764         Mvs PProductOnPltSet
765         Dly 0.3
766         M_Out(12256)=0              '�`���b�N��OFF
767         M_Out(12257)=1              '�`���b�N�JON
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
780     Mvs PProductOnJigSet_1      '������
781     Ovrd 30
782     Mvs PProductOnJigSet        '���i�u���ʒu
783     '
784     *RE_JIG_SET_2
785     '
786     M_Out(12258) = 0            '����i�`���b�N�JOFF
787     M_Out(12259) = 1            '����i�`���b�N��ON
788     '
789 '    Wait M_In(11271) = 1
790     MRtn = frInCheck(11271,1,MSETTIMEOUT05&)
791     If MRtn = 1 Then GoTo *CompJIGSet3
792     fErrorProcess(11,258,284,0)
793     If M_20# = MNext% Then M_20# = MClear%
794     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
795         M_Out(12256)=0              '�`���b�N��OFF
796         M_Out(12257)=1              '�`���b�N�JON
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
810     M_Out(12260)=0              '���i�N�����p�[�o�[OFF
811     M_Out(12261)=1              '���i�N�����p�[�����[ON
812 '    Wait M_In(11273)=1          '���i�N�����p�[�����[�Z���T�[ON
813     MRtn = frInCheck(11273,1,MSETTIMEOUT05&)   '����i�N�����p�[�����[�Z���T�[ON
814     If MRtn = 1 Then GoTo *CompJIGSet4
815     fErrorProcess(11,256,284,0)
816     If M_20# = MNext% Then M_20# = MClear%
817     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
818         M_Out(12256)=0              '�`���b�N��OFF
819         M_Out(12257)=1              '�`���b�N�JON
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
833     M_Out(12256)=0              '�`���b�N��OFF
834     M_Out(12257)=1              '�`���b�N�JON
835 '    Wait M_In(11265)=1          '�`���b�N�J�Z���T�[ON
836     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�`���b�N�J�Z���T�[ON
837     If MRtn = 1 Then GoTo *CompJIGSet5
838     fErrorProcess(11,244,284,0)
839     If M_20# = MNext% Then M_20# = MClear%
840     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
853     Mvs PProductOnJigSet_1      '������
854     Mvs PProductOnJigSet_2      '������_(�_�̒ǉ�(9�^2����))
855     '
856     *RE_JIG_SET_3
857     '
858 '    Wait M_In(11264)=0          '���i���o�Z���T�[OFF
859     MRtn = frInCheck(11264,0,MSETTIMEOUT05&)   '���i���o�Z���T�[OFF
860     If MRtn = 1 Then GoTo *CompJIGSet6
861     fErrorProcess(11,253,284,0)
862     If M_20# = MNext% Then M_20# = MClear%
863     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
878 '�˂����ߏ��ύX��������(6/9����)
879     *RE_SCREW_SET_1
880     '
881     M_Out(12257) = 0            '�`���b�N�JOFF(�l�W�s�b�N�A�b�v�����΍�9/10����)
882     M_Out(12256) = 1            '�`���b�N��ON(�l�W�s�b�N�A�b�v�����΍�9/10����)
883 '    Wait M_In(11266) = 1        '�`���b�N�Z���T�[ON
884     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)    '�`���b�N�Z���T�[ON
885     If MRtn = 1 Then GoTo *CompScrewSet1
886     fErrorProcess(11,245,284,0)
887     If M_20# = MNext% Then M_20# = MClear%
888     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
900 '�n���h�̌�����HS�˂����߂̌����ɕύX������W���K�v(6/9����)
901     Mov PScrewSupplyHS_5        '�n���h�����ς�
902     Mov PScrewSupplyHS_3        '����˂����{���_
903     '�q�[�g�V���N�̃l�W����
904     '
905     '�q�[�g�V���N�p�l�W�����@�փl�W�����ɍs��
906     'GoSub *ScrewSupplyHS         '�R�����g�A�E�g(9/3����)
907     '*ScrewSupplyHS               '�R�����g�A�E�g
908     PGetScrewPos(1) = PScrewSupplyHS_1  '�l�W�s�b�N�A�b�v���
909     PGetScrewPos(2) = PScrewSupplyHS_2  '�l�W�����@���_
910     PGetScrewPos(9) = PScrewSupplyHS_4  '�z���s�ǎ̂Ĉʒu
911     PGetScrewPos(10) = PScrewSupplyHS   '�l�W�s�b�N�A�b�v�ʒu
912 '    Mov PScrewSupplyHS_2          '�l�W�����@���_(9/30�ȉ�5�s�R�����g�A�E�g(����))
913 '    Mvs PScrewSupplyHS_1          '�l�W�s�b�N�A�b�v���(Mov����Mvs�֕ύX(9/3����))
914 '    Mvs PScrewSupplyHS            '�l�W�s�b�N�A�b�v
915 '    Mvs PScrewSupplyHS_1          '�l�W�s�b�N�A�b�v���
916 '    Mvs PScrewSupplyHS_2          '�l�W�����@���_(Mov����Mvs�֕ύX(9/3����))
917     *RE_SCREW_GET_1
918     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
919     If MRtn = 1 Then GoTo *CompScrewGet1
920     If M_20# = MNext% Then M_20# = MClear%
921     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
933     Mov PScrewSupplyHS_3          '����˂����{���_
934     'Return                       '�R�����g�A�E�g(9/3����)
935     '
936     '�@�ԃl�W����
937 '    Mov PScrewHeatSink1_1         '�@���(�ȉ�5�s�R�����g�A�E�g(9/30����))
938 '    Ovrd 5
939 '    Mvs PScrewHeatSink1           '�@�l�W����
940 '    Ovrd 10
941 '    Mvs PScrewHeatSink1_1         '�@���
942     PScrewPos(1) = PScrewHeatSink1_1    '�˂����ߏ��(�ȉ�4�s�ǉ�(9/30����))
943     PScrewPos(2) = PScrewHeatSink1_0    '�˂����ߊJ�n�ʒu
944     PScrewPos(10) = PScrewHeatSink1     '�˂����ߏI���ʒu
945     M_Out16(12672) = 1              '�l�W���߈ʒu�ԍ����M
946     MRtn = ScrewTight(PScrewPos,2,4.914)          '�˂����ߊJ�n
947     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
948     If MRtn = 1 Then GoTo *CompScrew1
949     Mov PScrewSupplyHS_3
950     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
951     MScrewErrorCord% = MScrewErrorCord% + 1
952     fErrorProcess(11,MScrewErrorCord%,52,0)
953 '    fErrorProcess(11,57,52,0)
954     If M_20# = MNext% Then M_20# = MClear%
955     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
967     '�q�[�g�V���N�p�l�W�����@�փl�W�����ɍs��
968     'GoSub *ScrewSupplyHS         '�R�����g�A�E�g(9/10����)
969     Mov PScrewSupplyHS_3          '����˂����{���_(�ȉ�3�s�ǉ�(9/30����))
970     *RE_SCREW_GET_2
971     MRtn = ScrewGet(PGetScrewPos , 11259 , 0)        '�l�W�󂯎��J�n
972     If MRtn = 1 Then GoTo *CompScrewGet2
973     If M_20# = MNext% Then M_20# = MClear%
974     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
986     Mov PScrewSupplyHS_3          '����˂����{���_
987     '
988     '�A�ԃl�W����
989 '    Mov PScrewHeatSink2_1         '�A���(�ȉ�5�s�R�����g�A�E�g(9/30����))
990 '    Ovrd 5
991 '    Mvs PScrewHeatSink2           '�A�l�W����
992 '    Ovrd 10
993 '    Mvs PScrewHeatSink2_1         '�A���
994     PScrewPos(1) = PScrewHeatSink2_1    '�˂����ߏ��(�ȉ�4�s�ǉ�(9/30����))
995     PScrewPos(2) = PScrewHeatSink2_0    '�˂����ߊJ�n�ʒu
996     PScrewPos(10) = PScrewHeatSink2     '�˂����ߏI���ʒu
997     M_Out16(12672) = 2              '�l�W���߈ʒu�ԍ����M
998     MRtn = ScrewTight(PScrewPos,6,4.914)          '�˂����ߊJ�n
999     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
1000     If MRtn = 1 Then GoTo *CompScrew2
1001     Mov PScrewSupplyHS_3
1002     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
1003     MScrewErrorCord% = MScrewErrorCord% + 2
1004     fErrorProcess(11,MScrewErrorCord%,52,0)
1005 '    fErrorProcess(11,58,52,0)
1006     If M_20# = MNext% Then M_20# = MClear%
1007     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1022     Mov PScrewSupplyPlate_3          '�l�W�s�b�N�A�b�v�������킹�ʒu
1023     '
1024 '    *RE_SCREW_SET_2
1025     '
1026     '���������ɂ�90�x��](���񏈗���4/13����)
1027 '    M_Out(12262) = 0             '��]�X�g�b�p�[�oOFF
1028 '    M_Out(12263) = 1             '��]�X�g�b�p�[��ON
1029 '    '
1030 ''    Wait M_In(11275) = 1         '��]�X�g�b�p�[�ߒ[���o�Z���T�[ON
1031 '    MRtn = frInCheck(11275,1,MSETTIMEOUT05&)   '�ʒu���ߖߒ[�Z���T�[ON
1032 '    If MRtn = 1 Then GoTo *CompScrewSet2
1033 '    fErrorProcess(11,262,284,0)
1034 '    If M_20# = MNext% Then M_20# = MClear%
1035 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1046     M_25# = 1                   '���񏈗��FCW��]�J�n(�f�o�b�O��)
1047 '
1048     '�����ʂ̃l�W����
1049     '
1050     '���p�l�W�����@�փl�W�����ɍs��
1051     'GoSub *ScrewSupplyPlate
1052     '*ScrewSupplyPlate
1053     PGetScrewPos(1) = PScrewSupplyPlate_1  '�l�W�s�b�N�A�b�v���
1054     PGetScrewPos(2) = PScrewSupplyPlate_2  '�l�W�����@���_
1055     PGetScrewPos(9) = PScrewSupplyPlate_7  '�z���s�ǖ߂��ʒu
1056     PGetScrewPos(10) = PScrewSupplyPlate   '�l�W�s�b�N�A�b�v�ʒu
1057 *RE_SCREW_GET_3
1058     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        '�l�W�󂯎��J�n
1059 '
1060     If MRtn = 1 Then GoTo *CompScrewGet3
1061     If M_20# = MNext% Then M_20# = MClear%
1062     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1074     Mov PScrewSupplyPlate_3          '�l�W�����@-����Ԓ��ԓ_(�ǉ�9/10����)
1075     '�^�N�g�Z�k�̂��ߒǉ��A�C��2/3����
1076 '    *RE_CW_ROT
1077 '    If M_20# = MContinue% Then
1078 '        M_Out(12264) = 1 Dly 0.3     'CW1�o���uON
1079 '    EndIf
1080 '    MRtn = frInCheck(11278,1,MSETTIMEOUT05&)   'CW�[�Z���T�[ON
1081 '    If MRtn = 1 Then GoTo *CompScrewSet3
1082 '    fErrorProcess(11,264,284,0)
1083 '    If M_20# = MNext% Then M_20# = MClear%
1084 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1085     M_20# = MClear%
1086 *LOOP1
1087     If M_20# = MContinue% Then
1088         M_25# = 1
1089         M_20# = MClear%
1090     EndIf
1091     If M_26# <> 0 Then GoTo *LOOP1END Else GoTo *LOOP1
1092 *LOOP1END
1093 '
1094     If M_26# = 1 Then GoTo *Set1End                            '����I���Ȃ�G���[�����Ȃ�
1095     If M_In(11275) = 1 And M_In(11278) = 1 Then GoTo *Set1End  '�����݂�����I���Ɠ����Z���T�[��ԂȂ�G���[�����Ȃ�
1096     If M_In(11275) = 0 Then                                    '�Z���T�[�̏�Ԃ����ăG���[���o��
1097         fErrorProcess(11,262,284,0)
1098     ElseIf M_In(11278) = 0 Then
1099         fErrorProcess(11,264,284,0)
1100     EndIf
1101     M_26# = 0
1102     If M_20# = MAbout% Or M_20# = MNgProcess% Then
1103         Mov PEscapePosition_3
1104         Mov PScrewSupplyPlate_7  '�z���s�ǖ߂��ʒu
1105         M_Out(12249)=1           '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1106         Dly 0.2
1107         '�j��ON
1108         M_Out(Y6B_VB1)=1 '�^��j��ON
1109         '�r�b�g��]
1110         M_Out(Y61_Driver)=1
1111         Dly 0.5
1112         '                '
1113         Ovrd 100
1114         JOvrd M_NJovrd
1115         Spd M_NSpd
1116         '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1117         Mov PScrewSupplyPlate_7,10
1118         Mov PScrewSupplyPlate_7
1119         Dly 0.1
1120         Mov PScrewSupplyPlate_7,10
1121         Mov PScrewSupplyPlate_7
1122 '
1123         '�l�W�����҂�
1124         Wait M_In(11268) = 0
1125         '�r�b�g��]��~
1126         M_Out(Y61_Driver)=0
1127         Dly 0.1
1128         '�j��OFF
1129         M_Out(Y6B_VB1)=0 '�^��j��OFF
1130         M_Out(12249)=0                  '�˂��z���@OFF
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
1144 '    M_26# = 0                       '�������^�C�~���O�ύX6/9����
1145 '
1146     Mov PScrewSupplyPlate_4          '������_
1147     'Return                          '�R�����g�A�E�g(9/3����)
1148     '
1149     '����L�l�W����(�d�l�ύX�ɂ�PScrewPlateL1��PScrewPlateL�֕ύX)
1150 '    Mov PScrewPlateL1_1            '�@���(�ȉ�5�s�R�����g�A�E�g(9/30����))
1151 '    Ovrd 5
1152 '    Mvs PScrewPlateL1              '�@�l�W����
1153 '    Ovrd 10
1154 '    Mvs PScrewPlateL1_1            '�@���
1155     PScrewPos(1) = PScrewPlateL1_1    '�˂����ߏ��(�ȉ�4�s�ǉ�(9/30����))
1156     PScrewPos(2) = PScrewPlateL1_0    '�˂����ߊJ�n�ʒu
1157     PScrewPos(10) = PScrewPlateL1     '�˂����ߏI���ʒu
1158     M_Out16(12672) = 3              '�l�W���߈ʒu�ԍ����M
1159     MRtn = ScrewTight(PScrewPos,4,4.914)          '�˂����ߊJ�n
1160     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
1161     If MRtn = 1 Then GoTo *CompScrew3
1162     Mov PScrewSupplyPlate_4
1163     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
1164     MScrewErrorCord% = MScrewErrorCord% + 3
1165     fErrorProcess(11,MScrewErrorCord%,52,0)
1166 '    fErrorProcess(11,53,52,0)
1167     If M_20# = MNext% Then M_20# = MClear%
1168     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1179     M_26# = 0                       '�������^�C�~���O�ύX6/9����
1180 '
1181 '    '�ȉ�17�s�d�l�ύX�ɂ��R�����g�A�E�g(11/4����)
1182 '    '���p�l�W�����@�փl�W�����ɍs��
1183 '    'GoSub *ScrewSupplyPlate       '�R�����g�A�E�g(9/9����)
1184     Mov PScrewSupplyPlate_4          '������_(�ȉ�5�s�ǉ�(9/30����))
1185     Mov PScrewSupplyPlate_3          '����-�l�W�����@�Ԓ��ԓ_
1186     *RE_SCREW_GET_4
1187     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        '�l�W�󂯎��J�n
1188     If MRtn = 1 Then GoTo *CompScrewGet4
1189     If M_20# = MNext% Then M_20# = MClear%
1190     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1202     Mov PScrewSupplyPlate_3          '�l�W�����@-����Ԓ��ԓ_
1203     Mov PScrewSupplyPlate_4          '������_
1204 '    '
1205 '    '�A�ԃl�W����
1206 ''    Mov PScrewPlateL2_1            '�A���(�ȉ�5�s�R�����g�A�E�g(9/30����))
1207 ''    Ovrd 5
1208 ''    Mvs PScrewPlateL2              '�A�l�W����
1209 ''    Ovrd 10
1210 ''    Mvs PScrewPlateL2_1            '�A���
1211     PScrewPos(1) = PScrewPlateL2_1    '�˂����ߏ��(�ȉ�4�s�ǉ�(9/30����))
1212     PScrewPos(2) = PScrewPlateL2_0    '�˂����ߊJ�n�ʒu
1213     PScrewPos(10) = PScrewPlateL2     '�˂����ߏI���ʒu
1214     M_Out16(12672) = 4              '�l�W���߈ʒu�ԍ����M
1215     MRtn = ScrewTight(PScrewPos,4,4.914)          '�˂����ߊJ�n
1216     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
1217     If MRtn = 1 Then GoTo *CompScrew4
1218     Mov PScrewSupplyPlate_4
1219     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
1220     MScrewErrorCord% = MScrewErrorCord% + 4
1221     fErrorProcess(11,MScrewErrorCord%,52,0)
1222 '    fErrorProcess(11,54,52,0)
1223     If M_20# = MNext% Then M_20# = MClear%
1224     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1236     Mov PScrewSupplyPlate_4        '�����]�ɂ����炩���߉��������(�ǉ�9/10����)
1237     '
1238     '
1239 '    *RE_SCREW_SET_3
1240 '    '
1241 '    M_Out(12265) = 1 Dly 0.3       'CW2�o���uON
1242 ''    Wait M_In(11277) = 1           'CCW�Z���^�[���o�Z���T�[ON
1243 '    MRtn = frInCheck(11277,1,MSETTIMEOUT05&)
1244 '    If MRtn = 1 Then GoTo *CompScrewSet4
1245 '    fErrorProcess(11,265,284,0)
1246 '    If M_20# = MNext% Then M_20# = MClear%
1247 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1261 '    M_Out(12266) = 1 Dly 0.3       'CCW1�o���uON
1262 '    Wait M_In(11279) = 1           'CCW�[���o�Z���T�[ON
1263 '    MRtn = frInCheck(11279,1,MSETTIMEOUT05&)   'CCW�[���o�Z���T�[ON(�^�N�g�Z�k�̂��߃R�����g�A�E�g2/3����)
1264 '    If MRtn = 1 Then GoTo *CompScrewSet5
1265 '    fErrorProcess(11,266,284,0)
1266 '    If M_20# = MNext% Then M_20# = MClear%
1267 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1279     '�E���ʂ̃l�W����
1280     '
1281     '���p�l�W�����@�փl�W�����ɍs��
1282     'GoSub *ScrewSupplyPlate       '�R�����g�A�E�g(9/9����)
1283     Mov PScrewSupplyPlate_3          '����-�l�W�����@�Ԓ��ԓ_(�ȉ�4�s�ǉ�(9/30����))
1284     '����180�x��]
1285     M_25# = 2
1286     *RE_SCREW_GET_5
1287     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        '�l�W�󂯎��J�n
1288     If MRtn = 1 Then GoTo *CompScrewGet5
1289     If M_20# = MNext% Then M_20# = MClear%
1290     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1301     Mov PScrewSupplyPlate_3          '�l�W�����@-����Ԓ��ԓ_
1302 '
1303 '    *RE_CCW_ROT
1304 '    If M_20# = MContinue% Then
1305 '        M_Out(12266) = 1 Dly 0.3     'CW1�o���uON
1306 '    EndIf
1307 '    'CCW�Z���T�[���o(�^�N�g�Z�k�̂��߈ړ��A�C���j
1308 '    MRtn = frInCheck(11279,1,MSETTIMEOUT05&)   'CCW�[���o�Z���T�[ON
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
1321     If M_26# = 1 Then GoTo *Set2End                            '����I���Ȃ�G���[�����Ȃ�
1322     If M_In(11279) = 1 Then GoTo *Set2End  '�����݂�����I���Ɠ����Z���T�[��ԂȂ�G���[�����Ȃ�
1323     If M_In(11277) = 0 Then                                    '�Z���T�[�̏�Ԃ����ăG���[���o��
1324         fErrorProcess(11,265,284,0)
1325     ElseIf M_In(11279) = 0 Then
1326         fErrorProcess(11,266,284,0)
1327     EndIf
1328     M_26# = 0
1329     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1330         Mov PEscapePosition_3
1331         Mov PScrewSupplyPlate_7  '�z���s�ǖ߂��ʒu
1332         M_Out(12249)=1       '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
1333         Dly 0.2
1334         '�j��ON
1335         M_Out(Y6B_VB1)=1 '�^��j��ON
1336         '�r�b�g��]
1337         M_Out(Y61_Driver)=1
1338         Dly 0.5
1339         '                '
1340         Ovrd 100
1341         JOvrd M_NJovrd
1342         Spd M_NSpd
1343         '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
1344         Mov PScrewSupplyPlate_7,10
1345         Mov PScrewSupplyPlate_7
1346         Dly 0.1
1347         Mov PScrewSupplyPlate_7,10
1348         Mov PScrewSupplyPlate_7
1349 '
1350         '�l�W�����҂�
1351         Wait M_In(11268) = 0
1352         '�r�b�g��]��~
1353         M_Out(Y61_Driver)=0
1354         Dly 0.1
1355         '�j��OFF
1356         M_Out(Y6B_VB1)=0 '�^��j��OFF
1357         M_Out(12249)=0                  '�˂��z���@OFF
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
1370 '    M_26# = 0                       '�������^�C�~���O�ύX6/9����
1371 '
1372     Mov PScrewSupplyPlate_4          '������_
1373     '
1374     '�@�ԃl�W����(�d�l�ύX�ɂ�PScrewPlateR1��PScrewPlateR�֕ύX)
1375 '    Mov PScrewPlateR1_1            '�@���(�ȉ�5�s�R�����g�A�E�g(9/30����))
1376 '    Ovrd 5
1377 '    Mvs PScrewPlateR1              '�@�l�W����
1378 '    Ovrd 10
1379 '    Mvs PScrewPlateR1_1            '�@���
1380     PScrewPos(1) = PScrewPlateR1_1    '�˂����ߏ��(�ȉ�4�s�ǉ�(9/30����))
1381     PScrewPos(2) = PScrewPlateR1_0    '�˂����ߊJ�n�ʒu
1382     PScrewPos(10) = PScrewPlateR1     '�˂����ߏI���ʒu
1383     M_Out16(12672) = 5              '�l�W���߈ʒu�ԍ����M
1384     MRtn = ScrewTight(PScrewPos,4,4.914)          '�˂����ߊJ�n
1385     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
1386     If MRtn = 1 Then GoTo *CompScrew5
1387     Mov PScrewSupplyPlate_4
1388     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
1389     MScrewErrorCord% = MScrewErrorCord% + 5
1390     fErrorProcess(11,MScrewErrorCord%,52,0)
1391 '    fErrorProcess(11,55,52,0)
1392     If M_20# = MNext% Then M_20# = MClear%
1393     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1404     M_26# = 0                       '�������^�C�~���O�ύX6/9����
1405 '
1406 '
1407     '�ȉ�18�s�d�l�ύX�ɂ��R�����g�A�E�g(11/4����)
1408 '    '���p�l�W�����@�փl�W�����ɍs��
1409 '    'GoSub *ScrewSupplyPlate       '�R�����g�A�E�g(9/9����)
1410     Mov PScrewSupplyPlate_4          '������_(�ȉ�5�s�ǉ�(9/30����))
1411     Mov PScrewSupplyPlate_3          '����-�l�W�����@�Ԓ��ԓ_
1412     '
1413     *RE_SCREW_GET_6
1414     '
1415     MRtn = ScrewGet(PGetScrewPos , 11260 , 0)        '�l�W�󂯎��J�n
1416     If MRtn = 1 Then GoTo *CompScrewGet6
1417     If M_20# = MNext% Then M_20# = MClear%
1418     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1429     Mov PScrewSupplyPlate_3          '�l�W�����@-����Ԓ��ԓ_
1430     Mov PScrewSupplyPlate_4          '������_
1431 '    '
1432 '    '�A�ԃl�W����
1433 ''    Mov PScrewPlateR2_1            '�A���(�ȉ�5�s�R�����g�A�E�g(9/30����))
1434 ''    Ovrd 5
1435 ''    Mvs PScrewPlateR2              '�A�l�W����
1436 ''    Ovrd 10
1437 ''    Mvs PScrewPlateR2_1            '�A���
1438     PScrewPos(1) = PScrewPlateR2_1    '�˂����ߏ��(�ȉ�4�s�ǉ�(9/30����))
1439     PScrewPos(2) = PScrewPlateR2_0    '�˂����ߊJ�n�ʒu
1440     PScrewPos(10) = PScrewPlateR2     '�˂����ߏI���ʒu
1441     M_Out16(12672) = 6              '�l�W���߈ʒu�ԍ����M
1442     MRtn = ScrewTight(PScrewPos,4,4.914)          '�˂����ߊJ�n
1443     M_Out16(12672) = 0              '�l�W���߈ʒu�ԍ��N���A
1444     If MRtn = 1 Then GoTo *CompScrew6
1445     Mov PScrewSupplyPlate_4
1446     MScrewErrorCord% = FnScreEroorCord()         '�G���[�R�����g�ɓd�h���G���[�R�[�h�ǉ� 22.05.23 �n��
1447     MScrewErrorCord% = MScrewErrorCord% + 6
1448     fErrorProcess(11,MScrewErrorCord%,52,0)
1449 '    fErrorProcess(11,56,52,0)
1450     If M_20# = MNext% Then M_20# = MClear%
1451     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1463     Mov PScrewSupplyHS_3           '����˂����{���_
1464     '
1465     '����90�x��]
1466 '    *RE_CENTER_POS
1467 '    M_Out(12267) = 1 Dly 0.3       'CCW2�o���uON
1468 ''    Wait M_In(11276) = 1           'CW�Z���^�[���o
1469 '    MRtn = frInCheck(11276,1,MSETTIMEOUT05&)   'CW�Z���^�[���o
1470 '    If MRtn = 1 Then GoTo *CompSenterPos1
1471 '    fErrorProcess(11,265,284,0)
1472 '    If M_20# = MNext% Then M_20# = MClear%
1473 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1484 '    M_Out(12263) = 0               '��]�X�g�b�p�[��OFF
1485 '    M_Out(12262) = 1               '��]�X�g�b�p�[�oON
1486 '    '
1487 ''    Wait M_In(11274) = 1           '��]�X�g�b�p�[�o�[���oON
1488 '    MRtn = frInCheck(11274,1,MSETTIMEOUT05&)   '��]�X�g�b�p�[�o�[���oON
1489 '    If MRtn = 1 Then GoTo *CompSenterPos2
1490 '    fErrorProcess(11,263,284,0)
1491 '    If M_20# = MNext% Then M_20# = MClear%
1492 '    If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1504 '    '����琻�i�����o��
1505 '    '
1506 ''    Mov PProductOnJigGet_6          '�l�W�s�b�N�A�b�v�������킹�ʒu
1507     M_25# = 3       '�ʒu�b��
1508     Mov PProductOnJigGet_5          '���i�`���b�N-�l�W�s�b�N�A�b�v�������킹���Ԉʒu
1509 '
1510     Mov PProductOnJigGet_4          '���i�s�b�N�A�b�v�������킹�ʒu
1511     M_20# = MClear%
1512 *LOOP3
1513     If M_20# = MContinue% Then
1514         M_25# = 3
1515         M_20# = MClear%
1516     EndIf
1517     If M_26# <> 0 Then GoTo *LOOP3END Else GoTo *LOOP3
1518 *LOOP3END
1519     If M_26# = 1 Then GoTo *Set3End                            '����I���Ȃ�G���[�����Ȃ�
1520     If M_In(11274) = 1 And M_In(11276) = 1 Then GoTo *Set3End  '�����݂�����I���Ɠ����Z���T�[��ԂȂ�G���[�����Ȃ�
1521     If M_In(11276) = 0 Then                                    '�Z���T�[�̏�Ԃ����ăG���[���o��
1522         fErrorProcess(11,265,284,0)
1523     ElseIf M_In(11274) = 0 Then
1524         fErrorProcess(11,263,284,0)
1525     EndIf
1526     M_26# = 0
1527     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1543 '�˂����ߏ��ύX�����܂�(6/9����)
1544     '
1545     *RE_JIG_GET_1
1546     '
1547     M_Out(12259) = 0                '����i�`���b�N��OFF
1548     M_Out(12258) = 1                '����i�`���b�N�JON
1549     M_Out(12261) = 0                '���i�N�����p�[�����[OFF
1550     M_Out(12260) = 1                '���i�N�����p�[�o�[ON
1551     M_Out(12256)= 0                 '���i�`���b�N��OFF
1552     M_Out(12257)= 1                 '���i�`���b�N�JON
1553     '
1554 '    Wait M_In(11265)=1              '���i�`���b�N�J�Z���T�[ON
1555     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '���i�`���b�N�J�Z���T�[ON
1556     If MRtn = 1 Then GoTo *CompJigGet1
1557     fErrorProcess(11,244,284,0)
1558     If M_20# = MNext% Then M_20# = MClear%
1559     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1571 '    Wait M_In(11272)=1              '����i�N�����p�[�Z���T�[�J���[ON
1572     MRtn = frInCheck(11272,1,MSETTIMEOUT05&)   '����i�N�����p�[�Z���T�[����[ON
1573     If MRtn = 1 Then GoTo *CompJigGet2
1574     fErrorProcess(11,257,284,0)
1575     If M_20# = MNext% Then M_20# = MClear%
1576     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1592     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
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
1605     'Mov PProductOnJigGet_3          '�n���h����](�R�����g�A�E�g9/10����)
1606     Mov PProductOnJigGet_2          '��������_
1607     Mvs PProductOnJigGet_1          '������
1608     Ovrd 30
1609     Mvs PProductOnJigGet            '���i���o���ʒu
1610     *RETRY_PRODUCT_ON_JIG_GET_2
1611     M_Out(12257)=0                  '���i�`���b�N�JOFF
1612     M_Out(12256)=1                  '���i�`���b�N��ON
1613 '    Wait M_In(11266)=1              '���i�`���b�N�Z���T�[ON
1614     MRtn = frInCheck(11266,1,MSETTIMEOUT05&)   '���i�`���b�N�Z���T�[ON
1615     If MRtn = 1 Then GoTo *CompJigGet4
1616 '
1617     fErrorProcess(11,245,284,0)
1618     If M_20# = MNext% Then M_20# = MClear%
1619     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1620         M_Out(12256)=0                  '���i�`���b�N��OFF
1621         M_Out(12257)=1                  '���i�`���b�N�JON
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
1636 '    Wait M_In(11264)=1              '���i���o�Z���T�[ON
1637     MRtn = frInCheck(11264,1,MSETTIMEOUT05&)   '���i���o�Z���T�[ON
1638     If MRtn = 1 Then GoTo *CompJigGet5
1639     fErrorProcess(11,252,284,0)
1640     If M_20# = MNext% Then M_20# = MClear%
1641     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1642         M_Out(12256)=0                  '���i�`���b�N��OFF
1643         M_Out(12257)=1                  '���i�`���b�N�JON
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
1660     Mvs PProductOnJigGet_1          '������
1661     Accel 100 , 100
1662     Ovrd 100
1663     Mvs PProductOnJigGet_2          '��������_
1664     Mov PProductOnJigGet_3          '�p���b�g-����ԓ_
1665     '
1666     '���i���p���b�g�ɒu��
1667     '
1668     Ovrd 60
1669     Mov PProductOnPltSet_3          '�ʉߓ_
1670     Mov PProductOnPltSet_2          '�p���b�g���_
1671     Ovrd 100
1672     Mov PProductOnPltSet_1          '�p���b�g���
1673     Ovrd 10
1674     Mvs PProductOnPltSet            '�p���b�g�u���ʒu
1675     Dly 0.2
1676     '
1677     *RE_PLT_SET_1
1678     '
1679     M_Out(12256)=0                  '���i�`���b�N��OFF
1680     M_Out(12257)=1                  '���i�`���b�N�JON
1681     '
1682 '    Wait M_In(11265)=1              '���i�`���b�N�J�Z���T�[ON
1683     MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '���i�`���b�N�J�Z���T�[ON
1684     If MRtn = 1 Then GoTo *CompPltSet1
1685     fErrorProcess(11,244,284,0)
1686     If M_20# = MNext% Then M_20# = MClear%
1687     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1688         Mvs PProductOnPltSet_1          '�p���b�g���
1689         Mov PProductOnPltSet_2          '�p���b�g���_
1690         Mov PInitialPosition
1691         Break
1692     EndIf
1693     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1694     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1695     If M_20# = MContinue% Then GoTo *RE_PLT_SET_1
1696     *CompPltSet1
1697     '
1698     Ovrd 100
1699     Mvs PProductOnPltSet_1          '�p���b�g���
1700     Mov PProductOnPltSet_2          '�p���b�g���_
1701     '
1702     *RE_PLT_SET_2
1703     '
1704 '    Wait M_In(11264) = 0            '���i���o�Z���T�[OFF
1705     MRtn = frInCheck(11264,0,MSETTIMEOUT05&)   '���i���o�Z���T�[OFF
1706     If MRtn = 1 Then GoTo *CompPltSet2
1707     fErrorProcess(11,253,284,0)
1708     If M_20# = MNext% Then M_20# = MClear%
1709     If M_20# = MAbout% Or M_20# = MNgProcess% Then  '�ޔ����[�g
1710         Mov PInitialPosition
1711         Break
1712     EndIf
1713     If M_20# = MAbout% Then GoTo *ASSY_ERROR_END
1714     If M_20# = MNgProcess% Then GoTo *ASSY_ERROR_END
1715     If M_20# = MContinue% Then GoTo *RE_PLT_SET_2
1716     *CompPltSet2
1717     '
1718 '    Mov PInitialPosition            '�v���O�������_
1719     MRtn = FnCtlValue2(2)       '�g���n�j�{�P  2022/04/28 �n��
1720     Mov PTicketRead_1
1721     MRtn = FnCtlValue2(99)      '�Ǐ��J�n�M��OFF  2022/04/28 �n��
1722     M_20# = MAssyOK%          '����I��
1723 '
1724 *ASSY_ERROR_END
1725     M_Out(12268) = 0            '�ʒu���ߏoOFF
1726     M_Out(12269) = 1            '�ʒu���ߖ�ON
1727 *AssyEnd
1728 *fnAssyStart_FEndPosi
1729     Exit Function
1730 FEnd
1731 '
1732 '��fnPiasCheck
1733 ''' <summary>
1734 ''' PIAS�`�P�b�g�Ǎ���
1735 ''' </summary>
1736 ''' <returns>   0 : NG
1737 '''             1 : OK(�Ǎ��݊���)
1738 ''' </returns>
1739 ''' <remarks>
1740 ''' Date   : 2021/07/07 : M.Hayakawa
1741 ''' </remarks>'
1742 Function M% fnPiasCheck
1743     fnPiasCheck = 0
1744     M_Out16(12576) = 79             'AUTO��� PIAS�`�P�b�g�Ǎ���
1745     Wait M_In(MIN_IS_Ready%) = 1            '�J�����ڑ�����(M5370)
1746 '
1747 *RETRY_PIAS
1748     M_20# = MClear%
1749     M_Out16(12576) = 80             'AUTO��� PIAS�`�P�b�g�Ǎ���
1750     '
1751     '�yID�`�P�b�g�ǂݍ��݁z
1752     PInspPosition(1) = PTicketRead  'ID�`�P�b�g�ǎ�ʒu
1753     MInspGroup%(1) = 1              '����G�ԍ�
1754     MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '�摜�����������s
1755 '
1756     '�G���[�̏ꍇ
1757     If MRtn <> 1 Then
1758         MRtn = ISInspectionSingle(PInspPosition, MInspGroup%, 1, -1, 1 )  '������x�摜�����������s
1759         If MRtn <> 1 Then
1760             'D720 -> D1300 �R�s�[�v��
1761             M_Out(12565) = 1
1762             Dly 0.5
1763             M_Out(12565) = 0
1764             '�G���[�����L�q
1765             fnWindScreenOpen(MWindErrScr3, 17, 20, 0)
1766             'GOT KEY���͑҂�
1767             MKeyNumber = fnKEY_WAIT()
1768             '
1769             Select MKeyNumber
1770                 Case MNext%         '���ւ�I�������ꍇ
1771                     M_20# = MPass%                          'M_20# �v���O�����ԋ��ʊO���ϐ�
1772                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1773                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1774                     Break
1775                 Case MAbout%        '��~��I�������ꍇ
1776                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1777                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1778                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1779                     Break
1780                 Case MNgProcess%    'NG��I�������ꍇ
1781                     M_20# = MAbout%                         'M_20# �v���O�����ԋ��ʊO���ϐ�
1782                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1783                     GoTo *fnPiasCheck_End                   'PIAS�`�F�b�N�I��
1784                     Break
1785                 Case MContinue%     '�p����I�������ꍇ
1786                     fnWindScreenOpen(MWindReSet, 0, 0, 0)   '�G���[��ʏ���
1787                     M_20# = MContinue%
1788                     GoTo *RETRY_PIAS                        'PIAS�`�F�b�N���g���C
1789                     Break
1790             End Select
1791         EndIf
1792     EndIf
1793 '----------D720 -> D1300 �R�s�[�v��----------
1794     M_Out(12565) = 1
1795     Dly 0.5
1796     M_Out(12565) = 0
1797 '----------�ʐM�m�F������----------
1798     fnAutoScreenComment(81) ' AUTO��� PC�ʐM�m�F
1799     MRtn = 0                ' ������
1800     M_20# = MClear%         ' ������
1801     MRtn = fnPCComuCheck()  ' PC-PLC�ʐM�`�F�b�N�iM_20# = MClear%/MAbout%/MNext%/MContinue%/MNgProcess%)
1802     ' �ʐM�m�FNG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1803     If MRtn <> 1 Then
1804         If M_20# = MContinue% Then
1805             GoTo *RETRY_PIAS         ' �`�P�b�g�ǂݒ������烊�g���C
1806         Else
1807             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1808         EndIf
1809     EndIf
1810 '----------�H�������m�F----------
1811     fnAutoScreenComment(82) ' AUTO��� �H�������m�F
1812     MRtn = 0                ' ������
1813     M_20# = MClear%         ' ������
1814     MRtn = fnProcessCheck() ' �H���t���O�`�F�b�N�iM_20# = MClear%/MAbout%/MContinue%/MNgProcess%M/Pass%)
1815     ' �H������NG���ɊO���ϐ��̏�Ԃɂ�胉�x���W�����v��������iOK���͂Ȃɂ����Ȃ��j
1816     If MRtn <> 1 Then
1817         If M_20# = MContinue% Then
1818             GoTo *RETRY_PIAS         ' ���g���C�̓`�P�b�g�ǂݒ�������
1819         Else
1820             GoTo *fnPiasCheck_End    ' ���̑���PIAS�`�F�b�N�I��
1821         EndIf
1822     EndIf
1823     '
1824     fnPiasCheck = 1
1825     *fnPiasCheck_End
1826     Exit Function
1827 FEnd
1828 '
1829 '��fnPCComuCheck
1830 ''' <summary>
1831 ''' PC-PLC�ʐM�`�F�b�N
1832 ''' </summary>
1833 ''' <returns>   0 : NG
1834 '''             1 : OK(�Ǎ��݊���)
1835 ''' </returns>
1836 ''' <remarks>
1837 ''' Date   : 2021/07/07 : M.Hayakawa
1838 ''' </remarks>'
1839 Function M% fnPCComuCheck
1840     fnPCComuCheck = 0
1841     MJudge% = 0                                  '������
1842     M_Out(MOUT_PIAS_ComCheck%) = 1               '12544 M6544 toPLC_PC�ʐM�m�F�v��(M300)
1843     Wait M_In(11575) = 1                         'M5575  toRBT_�ʐM�m�F�����ԐM
1844     '
1845     For MStaNo = 0 To 5
1846         '
1847         If M_In(MIN_PIAS_ComOK%) = 1 Then
1848             'PC�ʐMOK(M400)
1849             MJudge% = MOK%
1850             MStaNo = 5
1851             Break
1852         ElseIf M_In(MIN_PIAS_ComTimeOut%) = 1 Then
1853             'toRBT_�ʐM�m�Ftime out
1854             MJudge% = MNG%
1855             MCommentD1001 = 15
1856             MCommentD1002 = 21
1857             MStaNo = 5
1858             Break
1859         Else
1860             'toRBT_�ʐM�m�Ftime out
1861             MJudge% = MNG%
1862             MCommentD1001 = 14
1863             MCommentD1002 = 21
1864             Break
1865         EndIf
1866     Next MStaNo
1867     '
1868     '��L�ŕԐM�t���O����M���Ă���PC�ʐM�m�FOFF
1869     M_Out(MOUT_PIAS_ComCheck%) = 0               'PLC����M300��ێ����Ă���̂�RBT�ł͉���
1870     '
1871     '�G���[���
1872     If MJudge% <> MOK% Then
1873         M_20# = MClear%     '������
1874         '�G���[�����L�q
1875         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1876         'GOT KEY���͑҂�
1877         MKeyNumber = fnKEY_WAIT()
1878         '
1879         If MKeyNumber = MAbout% Then            '��~��I�������ꍇ
1880             M_20# = MAbout%                     'M_20# �v���O�����ԋ��ʊO���ϐ�
1881             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1882             Break
1883         ElseIf MKeyNumber = MNext% Then         '���ւ�I�������ꍇ
1884             M_20# = MNext%                      'M_20# �v���O�����ԋ��ʊO���ϐ�
1885             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1886             Break
1887         ElseIf MKeyNumber = MContinue% Then     '��~��I�������ꍇ
1888             M_20# = MContinue%                  'M_20# �v���O�����ԋ��ʊO���ϐ�
1889             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1890             Break
1891         ElseIf MKeyNumber = MNgProcess% Then    '���ւ�I�������ꍇ
1892             M_20# = MNgProcess%                 'M_20# �v���O�����ԋ��ʊO���ϐ�
1893             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
1894             Break
1895         EndIf
1896     Else
1897         'OK�̏ꍇ
1898         fnPCComuCheck = 1
1899     EndIf
1900     Exit Function
1901 FEnd
1902 '
1903 '��fnProcessCheck
1904 ''' <summary>
1905 ''' �H�������m�F
1906 ''' </summary>
1907 ''' <returns>    1�F�H������OK     0�F�ُ�I��
1908 '''             -1�F�O�H������NG  -2�F���H����������
1909 '''             -3�F���f���d��NG  -4�F�^�C���A�E�g
1910 '''             -5�F���������G���[
1911 ''' </returns>
1912 ''' <remarks>
1913 ''' Date   : 2021/07/07 : M.Hayakawa
1914 ''' </remarks>'
1915 Function M% fnProcessCheck
1916     fnProcessCheck = 0
1917     MJudge% = MNG%      '��UNG���������Ƃ���
1918 '----------�H�������m�F----------
1919     MCommentD1001 = 0   '�R�����g������
1920     For MStaNo = 0 To 5
1921         M_Out(MOUT_PIAS_Missing_Process%) = 1           'toPLC_PC�H�������m�F�v��(M302)
1922         Wait M_In(11577) = 1                            'M5577  toRBT_PC�H�������m�F�����ԐM
1923         '
1924         If M_In(MIN_PIAS_ProcessHistryOK%) = 1 Then             '11556 ����OK M407
1925             MJudge% = MOK%
1926             fnAutoScreenComment(85)     ' AUTO���
1927             MStaNo = 5
1928             Break
1929         ElseIf M_In(MIN_PIAS_MyProcessComp%) = 1 Then           '11573 ���H���������� M426
1930             MFlgLoop% = 0
1931             MJudge% = MNG%
1932             MCommentD1001 = 27
1933             MCommentD1002 = 22
1934             fnAutoScreenComment(94)     ' AUTO���
1935             fnProcessCheck = -2         ' NG��-2��Ԃ�
1936             MStaNo = 5
1937             Break
1938         ElseIf M_In(MIN_PIAS_ModelTypeNG%) = 1 Then             '11554 ���f���d��NG M406
1939            MJudge% = MNG%
1940             MCommentD1001 = 31
1941             MCommentD1002 = 22
1942             fnAutoScreenComment(83)     ' AUTO���
1943             fnProcessCheck = -3         ' NG��-3��Ԃ�
1944             MStaNo = 5
1945             Break
1946         ElseIf M_In(MIN_PIAS_ProcessHistryNG%) = 1 Then         '11555 �O�H������NG M408
1947             '����NG�͒����ɏI�������J��Ԃ��m�F���s��
1948             '�O�H���̏����݂��I�����Ă��Ȃ��\�������邽��
1949             MJudge% = MNG%
1950             MCommentD1001 = 32
1951             MCommentD1002 = 22
1952             fnAutoScreenComment(84)     ' AUTO���
1953             fnProcessCheck = -1         ' NG��-1��Ԃ�
1954             Dly 1.0
1955             '�H�������m�FOFF
1956             M_Out(MOUT_PIAS_Missing_Process%) = 0               'toPLC_PC�H�������m�F�v��(M302)
1957             Dly 1.0
1958            'MStaNo = 5
1959             Break
1960         ElseIf M_In(MIN_PIAS_ProcessHistryErr%) = 1 Then        '11557 ���������G���[ M432
1961             MFlgLoop% = 0
1962             MJudge% = MNG%
1963             MCommentD1001 = 29
1964             MCommentD1002 = 22
1965             fnAutoScreenComment(86)     ' AUTO��� ���������G���[
1966             fnProcessCheck = -5         ' NG��-5��Ԃ�
1967             MStaNo = 5
1968             Break
1969         ElseIf M_In(MIN_PIAS_ProcessHistryTimeOut%) = 1 Then    '�^�C���A�E�g
1970             MJudge% = MNG%
1971             If MCommentD1001 = 32 Then
1972                 '�������Ȃ�
1973             Else
1974                 MCommentD1001 = 26
1975             EndIf
1976             MCommentD1002 = 22
1977             fnProcessCheck = -4         ' NG��-4��Ԃ�
1978             MStaNo = 5
1979             Break
1980         Else
1981             MJudge% = MNG%
1982             MCommentD1001 = 28
1983             MCommentD1002 = 22
1984         EndIf
1985     Next MStaNo
1986     '�H�������m�FOFF
1987     M_Out(MOUT_PIAS_Missing_Process%) = 0      'toPLC_PC�H�������m�F�v��(M302)
1988     '�ʉߗ���NG �H�������̏ꍇ
1989     If MJudge% = MPass% Then
1990         M_20# = MPass%
1991     EndIf
1992     '
1993     '�G���[���
1994     If MJudge% <> MOK% Then
1995         M_20# = MClear%     '������
1996         '�G���[�����L�q
1997         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
1998         'GOT KEY���͑҂�
1999         MKeyNumber = fnKEY_WAIT()
2000         '
2001         Select MKeyNumber
2002             Case MAbout%        '��~��I�������ꍇ
2003                 M_20# = MAbout%         'M_20# �v���O�����ԋ��ʊO���ϐ�
2004                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2005                 Break
2006             Case MNext%         '���ւ�I�������ꍇ
2007                 M_20# = MPass%          'M_20# �v���O�����ԋ��ʊO���ϐ�
2008                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2009                 Break
2010             Case MContinue%     '�p����I�������ꍇ
2011                 M_20# = MContinue%      'M_20# �v���O�����ԋ��ʊO���ϐ�
2012                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2013                 Break
2014             Case MNgProcess%    'NG��I�������ꍇ
2015                 M_20# = MNgProcess%     'M_20# �v���O�����ԋ��ʊO���ϐ�
2016                 fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2017                 Break
2018         End Select
2019     Else
2020         fnProcessCheck = 1  ' OK��1��Ԃ�
2021     EndIf
2022     Exit Function
2023 FEnd
2024 '
2025 '��fnPiasWrite
2026 ''' <summary>
2027 ''' Pias �g�����ʏ����ݗv��
2028 ''' </summary>
2029 '''<param name="MFlg%">
2030 '''                 MOK%(1) = �H��������OK��������
2031 '''                 MNG%(0) = �H��������NG��������
2032 '''</param>
2033 '''<returns></returns>
2034 ''' <remarks>
2035 ''' Date   : 2021/07/07 : M.Hayakawa
2036 ''' </remarks>'
2037 Function M% fnPiasWrite(ByVal MFlg%)
2038       fnPiasWrite = 0
2039 *RETRY_PIASWRITE
2040     '
2041     '�g��OK(MOK%)�̏ꍇ�@M306 ON
2042    '�g��NG(MNG%)�̏ꍇ�@M307 ON
2043     If MFlg% = MOK% Then
2044         M_Out(MOUT_PiasAssyResultOK%) = 1     'M6549 -> M306
2045     Else
2046         M_Out(MOUT_PiasAssyResultNG%) = 1     'M6550 -> M307
2047     EndIf
2048     Dly 0.1                  '�O�̂���
2049     '
2050     'Pias�֏����݊J�n M305 -> ON
2051     M_Out(MOUT_PiasAssyResultWr%) = 1         'M6548 -> M305
2052     Wait M_In(11582) = 1                        '�g�����������ԐM M5582
2053     '
2054     MJudge% = MNG%
2055     '
2056     For MStaNo = 0 To 5
2057         If M_In(MIN_PiasProcessOK%) = 1 Then          'M414 �H����������OK
2058             MJudge% = MOK%
2059             'MRet = fnAutoScreenComment(85)  'AUTO���
2060             MStaNo = 5
2061             Break
2062         '
2063         ElseIf M_In(MIN_PiasProcessNG%) = 1 Then          'M415 �H����������NG
2064             MJudge% = MNG%
2065             'MRet = fnAutoScreenComment(85)  'AUTO���
2066            MCommentD1001 = 34
2067            MCommentD1002 = 25
2068             MStaNo = 5
2069             Break
2070         '
2071         ElseIf M_In(MIN_PiasProcessOtherErr%) = 1 Then         'M435 �H�����������G���[(�Ȃ񂩂̃g���u��)
2072             MJudge% = MNG%
2073             'MRet = fnAutoScreenComment(85)  'AUTO���
2074            MCommentD1001 = 35
2075            MCommentD1002 = 25
2076             MStaNo = 5
2077             Break
2078         '
2079         ElseIf M_In(11583) = 1 Then                         '�H����������time out
2080             MJudge% = MNG%
2081             'MRet = fnAutoScreenComment(85)  'AUTO���
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
2096     'Pias�֏����݊J�n M305 -> OfF
2097     M_Out(MOUT_PiasAssyResultWr%) = 0         'M6548 -> M305
2098     M_Out(MOUT_PiasAssyResultOK%) = 0     'M6549 -> M306
2099     M_Out(MOUT_PiasAssyResultNG%) = 0     'M6550 -> M307    '
2100     '
2101     '
2102     '�ʉߗ���NG �H�������̏ꍇ
2103     If MJudge% = MPass% Then
2104         M_20# = MPass%
2105     EndIf
2106     '
2107    M_20# = MClear%     '������
2108     '
2109     '�G���[���
2110     If MJudge% < MOK% Then
2111     '
2112 '�c���Ă���������ł͎g�p���Ȃ����x��
2113 *RETRY_ERR_WRITE
2114         M_20# = MClear%     '������
2115         '�G���[�����L�q
2116         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
2117         'GOT KEY���͑҂�
2118         MKeyNumber = fnKEY_WAIT()
2119         '
2120         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2121             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2122            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2123             Break
2124         '
2125         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2126             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2127             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2128         '
2129         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2130             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2131             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2132         '
2133         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2134             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2135            fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
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
2150 '��fnPCBNumberCheck
2151 ''' <summary>
2152 ''' Pias ��ԍ��ƍ��v��
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
2164     fnAutoScreenComment(91)  'AUTO��� ���񏑍���
2165     'Pias�֊�ƍ��J�n M310 -> ON
2166     M_Out(MOUT_PiasPCBNumberCheck%) = 1         'M6557 -> M310
2167     Wait M_In(11579) = 1                        '��ԍ������ԐM M5579
2168     '
2169     MJudge% = MNG%
2170     '
2171     For MStaNo = 0 To 5
2172         If M_In(MIN_PiasPCBNumberOK%) = 1 Then              'M420 ��ԍ�����OK
2173             MJudge% = MOK%
2174             fnAutoScreenComment(96)  'AUTO���
2175             MStaNo = 5
2176             Break
2177         '
2178         ElseIf M_In(MIN_PiasPCBNumberNG%) = 1 Then          'M421 ��ԍ�NG
2179             MJudge% = MNG%
2180             fnAutoScreenComment(97)  'AUTO���
2181             MCommentD1001 = 37
2182             MCommentD1002 = 25
2183             MStaNo = 5
2184             Break
2185         '
2186         ElseIf M_In(MIN_PiasPCBNumberErr%) = 1 Then         'M440 ��ԍ������G���[(�Ȃ񂩂̃g���u��)
2187             MJudge% = MNG%
2188             fnAutoScreenComment(98)  'AUTO���
2189             MCommentD1001 = 38
2190             MCommentD1002 = 25
2191             MStaNo = 5
2192             Break
2193         '
2194         ElseIf M_In(11580) = 1 Then                         'time out
2195             MJudge% = MNG%
2196             fnAutoScreenComment(99)  'AUTO���
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
2211     'Pias�֊�ƍ��J�n M310 -> OfF
2212     M_Out(MOUT_PiasPCBNumberCheck%) = 0         'M6557 -> M310
2213     '
2214     '
2215     '�ʉߗ���NG �H�������̏ꍇ
2216     If MJudge% = MPass% Then
2217         M_20# = MPass%
2218     EndIf
2219     '
2220    M_20# = MClear%     '������
2221     '
2222     '�G���[���
2223     If MJudge% < MOK% Then
2224     '
2225 '�c���Ă���������ł͎g�p���Ȃ����x��
2226 *RETRY_ERR_PCBNUMBER
2227         M_20# = MClear%     '������
2228         '�G���[�����L�q
2229         fnWindScreenOpen(MWindErrScr3, MCommentD1001, MCommentD1002, 0)
2230         'GOT KEY���͑҂�
2231         MKeyNumber = fnKEY_WAIT()
2232         '
2233         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
2234             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2235             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2236             Break
2237         '
2238         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
2239             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2240             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2241         '
2242         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
2243             M_20# = MPass%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2244             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
2245         '
2246         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
2247             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
2248             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
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
2261 '��ScrewTight
2262 ''' <summary>
2263 ''' �˂����߂��s��(S�^�C�g)
2264 ''' </summary>
2265 '''<param name="PScrewPos()">
2266 '''             PScrewPos(1)    �F�p���b�g��˂�����S�@�̈��S����ʒu  +30
2267 '''             PScrewPos(2)    �F�˂����߉��_
2268 '''             PScrewPos(10)   �F�˂����ߏI������
2269 '''<param name="MScrewType">�l�W�^�C�v(mm/sec)
2270 '''             1:6mm S�^�C�g��l�W
2271 '''             2:13mm S�^�C�g��l�W
2272 '''             3:6mm S�^�C�g���l�W
2273 '''             4:3mm S�^�C�g���l�W
2274 '''             5:6mm M�l�W
2275 '''             6:13mm S�^�C�g��l�W
2276 '''</param>
2277 '''<param name="MFeedSpd">���葬�x(mm/sec)</param>
2278 '''<returns>����
2279 '''         0=�ُ�I���A1=����I��
2280 '''</returns>
2281 ''' <remarks>
2282 ''' Date   : 2021/07/07 : M.Hayakawa
2283 ''' Update : 2021/09/28 : M.Hayakawa �l�W�^�C�v�A���葬�x�������ɒǉ�
2284 ''' </remarks>'
2285 Function M% ScrewTight(ByVal PScrewPosition(),ByVal MScrewType%,ByVal MFeedSpd)   '�l�W���ߌʐݒ�
2286     fnAutoScreenComment(521)    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2287     ScrewTight = 0
2288     MOKNGFlg = 0
2289     Ovrd 100
2290     Fine 0.05 , P
2291     Mvs PScrewPosition(1)       ' �p���b�g��˂�����S�@�̈��S����ʒu
2292     Select MScrewType%      '�ǂݍ��݈ʒu�ύX(1/19����)
2293         Case 1
2294             ' S�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
2295             ProgramBankSet(1,1)
2296             Break
2297         Case 2
2298             ' S�^�C�g13mm�F�v���O����2�A�o���N1�ɐݒ�
2299             ProgramBankSet(2,1)
2300             Break
2301         Case 3
2302             ' S�^�C�g���F�v���O����3�A�o���N1�ɐݒ�
2303             ProgramBankSet(3,1)
2304             Break
2305         Case 4
2306             ' S�^�C�g3mm���F�v���O����4�A�o���N1�ɐݒ�
2307             ProgramBankSet(4,1)
2308             Break
2309         Case 5
2310             ' M�l�W�F�v���O����5�A�o���N1�ɐݒ�
2311             ProgramBankSet(5,1)
2312             Break
2313         Case 6
2314             ' S�^�C�g13mm(�p�����[�^�Ⴂ):�v���O����2�A�o���N2�ɐݒ�
2315             ProgramBankSet(2,2)
2316             Break
2317         Default
2318             ' �v���O����1�A�o���N�Ȃ��ݒ�
2319             ProgramBankSet(0,0)
2320             Break
2321     End Select
2322     Accel 100,10
2323 '    Ovrd MOvrdA%               '10/7���ݒlNull
2324     Ovrd 60                     '�O�̂��ߌ��� ���l�ύX ��
2325     ' �p���b�g��˂����ߊJ�n�ʒu�ֈړ�
2326     Mvs PScrewPosition(2)
2327     ' ����Ovrd�ݒ�
2328 '    Ovrd MOvrdA%
2329     Ovrd 100
2330     Accel
2331     ' Spd�ݒ�
2332     Spd MFeedSpd * (100/M_Ovrd) * (100/M_OPovrd)
2333 '    Spd MFeedSpd
2334     ' �ݒ�i�ݗ�5.0 �~ ����p�l���̃I�[�o�[���C�h�W�� �~ �v���O�������I�[�o�[���C�h�W��
2335     ' Spd = 5 * (100/M_Ovrd) * (100/M_OPOvrd)
2336 '    Select MScrewType%      '�ǂݍ��݈ʒu�ύX(1/19����)
2337 '        Case 1
2338 '            ' S�^�C�g�F�v���O����1�A�o���N1�ɐݒ�
2339 '            ProgramBankSet(1,1)
2340 '            Break
2341 '        Case 2
2342 '            ' S�^�C�g13mm�F�v���O����2�A�o���N1�ɐݒ�
2343 '            ProgramBankSet(2,1)
2344 '            Break
2345 '        Case 3
2346 '            ' S�^�C�g���F�v���O����3�A�o���N1�ɐݒ�
2347 '            ProgramBankSet(3,1)
2348 '            Break
2349 '        Case 4
2350 '            ' S�^�C�g3mm���F�v���O����4�A�o���N1�ɐݒ�
2351 '            ProgramBankSet(4,1)
2352 '            Break
2353 '        Case 5
2354 '            ' M�l�W�F�v���O����5�A�o���N1�ɐݒ�
2355 '            ProgramBankSet(5,1)
2356 '            Break
2357 '        Default
2358 '            ' �v���O����1�A�o���N�Ȃ��ݒ�
2359 '            ProgramBankSet(0,0)
2360 '            Break
2361 '    End Select
2362 '
2363 '    Mvs PScrewPosition(2) Wth M_Out(Y61_Driver)=1     '�h���C�o�[ON�@CW
2364      '�h���C�o�[ON�@CW
2365     M_Out(12241)=1
2366     Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
2367     Wait M_In(11584)=1          '����/�G���[���o �b��R�����g 10/6 M.H
2368     Dly 0.1
2369     Spd M_NSpd
2370     Fine 0 , P
2371     '
2372     If M_In(11256)=1 Then  '�˂��g�[�^���G���[���o��
2373         M_Out(Y61_Driver)=0     '�h���C�o�[OFF�@CW
2374         Dly 0.1
2375        ' �v���O�����E�o���N����
2376         ProgramBankSet(0,0)
2377         '�p���b�g��˂����ߏI���ʒu���ֈړ�
2378         Mvs PScrewPosition(10),-80
2379         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
2380         M_Out(12249)=1 Dly 0.3
2381         MOKNGFlg = -1
2382         ScrewTight = 0
2383     Else
2384          '�h���C�o�[OFF�@CW
2385         M_Out(12241)=0
2386 ''        �G���[���Ȃ��ꍇ�̓l�W���ߏI���ʒu�ő�������
2387 '        Select MScrewType%
2388 '            Case 1
2389 '                ' S�^�C�g�F�v���O����1�A�o���N3�ɐݒ�
2390 '                ProgramBankSet(1,3)
2391 '                Break
2392 '            Case 2
2393 '                ' S�^�C�g13mm�F�v���O����2�A�o���N3�ɐݒ�
2394 '                ProgramBankSet(2,3)
2395 '                Break
2396 '            Case 3
2397 '                ' S�^�C�g���F�v���O����1�A�o���N3�ɐݒ�
2398 '                ProgramBankSet(3,3)
2399 '                Break
2400 '            Case 4
2401 '                ' S�^�C�g13mm�F�v���O����1�A�o���N3�ɐݒ�
2402 '                ProgramBankSet(4,3)
2403 '                Break
2404 '            Case 5
2405 '                ' M�l�W�F�v���O����1�A�o���N3�ɐݒ�
2406 '                ProgramBankSet(5,3)
2407 '                Break
2408 '            Default
2409 '                ' �v���O����1�A�o���N�Ȃ��ݒ�
2410 '                ProgramBankSet(0,0)
2411 '                Break
2412 '        End Select
2413 '         '�h���C�o�[ON�@CW
2414 '        Mvs PScrewPosition(10)
2415 '        M_Out(12241)=1
2416 '        Mvs PScrewPosition(10) WthIf M_In(11584)=1,Skip   '�˂����ߏI�������܂ňړ����G���[���o
2417 '
2418          '�h���C�o�[OFF�@CW
2419         M_Out(12241)=0
2420        ' �v���O�����E�o���N����
2421         ProgramBankSet(0,0)
2422         '�˂��z���@OFF'M_Out(Y68_VV1)=0����M_Out(12249)=1 Dly 0.3�֕ύX(8/27����)
2423         M_Out(12249)=1 Dly 0.3
2424     '     ��PScrewPos(2) �� PScrewPosition(10)�ɕύX 9/16 M.Hayakawa
2425         '�p���b�g��˂����ߏI���ʒu���ֈړ�
2426         Mvs PScrewPosition(10),-80
2427         ScrewTight = 1
2428     EndIf
2429 ' �b��i�b��}�X�N�@9/16 M.Hayakawa)
2430 '    Ovrd 10
2431 '    Mvs PScrewPosition(1)        ' �p���b�g��˂�����S�@�̈��S����ʒu
2432     Ovrd 100
2433     Exit Function
2434 FEnd
2435 '
2436 '��ScrewGet
2437 ''' <summary>
2438 ''' �˂������@����˂��𓾂�
2439 ''' </summary>
2440 '''<param name="%">
2441 '''         PScrewPos(1)    �F�˂�������̂˂����
2442 '''         PScrewPos(2)    �F�˂���������_
2443 '''         PScrewPos(9)    �F�˂���������l�W�̈ʒu
2444 '''         PScrewPos(10)   �F�˂�������̂˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2445 '''         PScrewPos(3)    �FM�˂��|�J���P�ʒu
2446 '''         PScrewPos(4)    �FM�˂��|�J���P�ʒu�@���
2447 '''</param>
2448 '''<param name = FeederReadyNo%> �w��̋����@Ready </param>
2449 '''<param name = FeederScrewSensor%> �w��̌닟���h�~�Z���T�[�w��(0�ŃZ���T�[����)</param>
2450 '''<returns>����
2451 '''         0=�ُ�I���A1=����I���A-1=�˂�����NG�A-2=�˂��닟��NG�A-3=�z���G���[
2452 '''</returns>
2453 ''' <remarks>
2454 ''' Date   : 2021/07/07 : M.Hayakawa
2455 ''' </remarks>
2456 '''<update>
2457 '''Date    : 2021/11/15 : ����
2458 '''</update>
2459 Function M% ScrewGet(ByVal PScrewPosition() , ByVal FeederReadyNo% , ByVal FeederScrewSensor%)
2460     fnAutoScreenComment(522)    '��ԕ\��[�l�W�����҂�] 2022/05/09 �n��
2461     ScrewGet = 0
2462     MScrewJudge% = 0
2463     '�˂������평������G���[�`�F�b�N
2464     Mov PScrewPosition(2)   '�˂������@���_�ֈړ�
2465     For MCnt% = 0 To MFinCnt%
2466         MRtn = frInCheck(FeederReadyNo% , 1 , MSETTIMEOUT05&)    '�w��˂������@��Ready�ɂȂ��Ă��邩�m�F(5�b��)
2467         If MRtn = 0 Then
2468             M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
2469             ScrewGet = -1
2470             MScrewJudge% = 2
2471         EndIf
2472         Ovrd 100
2473         If FeederScrewSensor% <> 0 Then
2474             If M_In(FeederScrewSensor%) = 1 Then  '�닟�������o���ꂽ��
2475                 'Ovrd 30
2476                 M_Out(12249)=1 Dly 0.3     '�˂��z�� Off(�O�̂���)
2477                 'NG�Ƃ��Ă����̊֐����甲����
2478                 ScrewGet = -2
2479                 MScrewJudge% = 3
2480             EndIf
2481         EndIf
2482         Ovrd 100
2483         Spd M_NSpd
2484         If MScrewJudge% = 0 Then
2485     '        ScrewGet = 0
2486             M_Out(Y63_Driver)=1         ' �o���N�Z�b�e�B���O�@C2
2487             Dly 0.3
2488             MScrewCnt% = 0
2489             MFinCnt% = 2
2490             fnAutoScreenComment(521)     '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2491             Mov PScrewPosition(1)        ' �˂������@(S�l�W�j���
2492             'Ovrd 40 '2�ɕύX 10/6 M.H '5�ɕύX10/7����
2493             '�˂�����(S�l�W�j�˂��s�b�N�A�b�v�ʒu�F�˂����ƃr�b�g�Ƃ̃N���A�����X��0.3mm�����z
2494             '�l�W�ƃr�b�g⻍������� �z���ʒu����1.2������⻍�
2495             'Mvs PScrewPosition(10), 1.2
2496            Mvs PScrewPosition(10)       'Fan�p�˂��z���ʒu�C���̂��ߕύX 2022-02-01AJI
2497             '�r�b�g��](�����ʒu�ύX)
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
2508             M_Out(Y68_VV1)=1 Dly 0.3     ' �˂��z���@ON'Dly 0.3�ǉ�(8/27����)
2509             '�z���m�F
2510             MRtn = 0
2511             MRtn = frScrewInCheck(11267, 1, MSETTIMEOUT01&)
2512 '            '�r�b�g��](�����ʒu�ύX)
2513 '            M_Out(Y60_Driver)=1
2514 '            Dly 0.2
2515             '
2516             JOvrd M_NJovrd
2517             Spd M_NSpd
2518             Ovrd 50
2519             '�l�W�z���m�F�ʒu�ړ�
2520             Mvs PScrewPosition(10)       ' �O�̂��߈�U�A���˂��z���ʒu
2521             Mvs PScrewPosition(1)        ' �l�W�z���m�F�ʒu(10),-30����(1)�֕ύX7/25����
2522             Ovrd 100
2523            'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
2524             '�r�b�g��]��~
2525             M_Out(Y60_Driver)=0
2526             '
2527 '            If MRtn = 1 Then           '�ŏ���臒l���m�F���Ȃ�
2528                 '1�b�ԃl�W�z���m�F
2529                 MRtn = frScrewInCheck(11268, 1, MSETTIMEOUT01&)
2530 '            MRtn = 1 '����OK
2531 '            EndIf
2532             'MRtn = 0'�����G���[
2533             '�z���G���[�̏ꍇ
2534             '�l�W���˂����Y�ɖ߂�
2535             If MRtn = 0 Then
2536                 Ovrd 30      '2����5�ɕύX
2537                 '�r�b�g��]��~
2538                 M_Out(Y60_Driver)=0
2539                 '�l�W�����@���
2540                 Mvs PScrewPosition(1)
2541                 '�X�ɏ��
2542                 Mov PScrewPosition(1), -140
2543                 '�l�W�̂Ĉʒu
2544                 If FeederReadyNo% = 11259 Then     '�����@�ʂɋz���G���[�����J�E���g�@2022/05/19 �n��
2545                     MRtn = FnCtlValue2(3)          '�����@�Q�z���G���[���{�P
2546                 Else
2547                     MRtn = FnCtlValue2(4)          '�����@�P�z���G���[���{�P  2022/04/28 �n��
2548                 EndIf
2549                 Mov PScrewPosition(9)
2550                 MRtn = FnCtlValue2(99)         '�Ǐ��J�n�M��OFF  2022/04/28 �n��
2551                 '�z��OFF
2552                 M_Out(12249)=1                  '�˂��z���@OFF(�p���X�o�͎����22/9/13����)
2553                 Dly 0.2
2554                 '�j��ON
2555                 M_Out(Y6B_VB1)=1 '�^��j��ON
2556                 '�r�b�g��]
2557                 M_Out(Y61_Driver)=1
2558                 Dly 0.5
2559                 '                '
2560                 Ovrd 100
2561                 JOvrd M_NJovrd
2562                 Spd M_NSpd
2563                 '�h���C�o�[���㉺�����˂���U�藎�Ƃ�
2564                 Mov PScrewPosition(9), 10
2565                 Mov PScrewPosition(9)
2566                 Dly 0.1
2567                 Mov PScrewPosition(9), 10
2568                 Mov PScrewPosition(9)
2569                 '
2570                 '�l�W�����҂�
2571                 Wait M_In(11272) = 0
2572                 '�r�b�g��]��~
2573                 M_Out(Y61_Driver)=0
2574                 Dly 0.1
2575                 '�j��OFF
2576                 M_Out(Y6B_VB1)=0 '�^��j��OFF
2577                 M_Out(12249)=0                  '�˂��z���@OFF
2578                 '�˂��������Ƃ��āA�ړ��X�ɏ��
2579                 Mov PScrewPosition(1), -140
2580                 Ovrd 100
2581                 Spd M_NSpd
2582                 '�l�W�����@���
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
2606 '        Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
2607 '        'Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
2608 '        M_Out(Y60_Driver)=0     ' �r�b�g��]��~
2609 '        M_Out(Y63_Driver)=0     ' �o���N�Z�b�e�B���O�@C2
2610 '        'Mvs PScrewPosition(10), -30  ' �˂��s�b�N�A�b�v�ʒu -30mm
2611 '        Mvs PScrewPosition(1)        ' �˂������@(S�l�W�j���
2612 '        'Mov PScrewPosition(2)
2613 '        '������x�z���m�F�@���̍ŏI臒l
2614 '        MRtn = frInCheck(11265, 1, MSETTIMEOUT01&)
2615 '        If MRtn = 0 Then      '�z���G���[�̏ꍇ
2616 '            MScrewJudge% = 4
2617 '            ScrewGet = -3
2618 '        ElseIf MRtn = 1 Then      '�z��OK�̏ꍇ
2619 '            MScrewJudge% = 1
2620 '            ScrewGet = 1
2621 '        EndIf
2622 '        Break
2623 '    EndIf
2624     '
2625     Mov PScrewPosition(2)
2626 '
2627 '    If MScrewJudge% = 1 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
2628     If MScrewJudge% = 0 Then GoTo *End_ScrewGet                 '����I�������x���ɃW�����v
2629     '
2630     Select MScrewJudge%
2631 '        Case 0
2632 ''            fErrorProcess(11,162,163,0) '�ُ�I��
2633 '            MCommentD1001 = 162
2634 '            MCommentD1002 = 96
2635 '            Break
2636         Case 2
2637 '            fErrorProcess(11,63,161,0) '����NG
2638             MCommentD1001 = 63
2639             MCommentD1002 = 96
2640             Break
2641         Case 3
2642 '            fErrorProcess(11,160,164,0) '�닟��
2643             MCommentD1001 = 237
2644             MCommentD1002 = 96
2645             Break
2646         Case 4
2647 '            fErrorProcess(11,94,95,0) '�z��NG
2648             MCommentD1001 = 94
2649             MCommentD1002 = 95
2650             Break
2651     End Select
2652     fErrorProcess(11,MCommentD1001,MCommentD1002,0)
2653     '
2654     Select M_20#
2655         Case MAbout%          '��~�������ꂽ�ꍇ
2656             Mov PScrewPosition(2)                  '���_�Ɉړ����Ċ֐��𔲂���
2657             Break
2658         Case MContinue%       '���g���C��������Ă����ꍇ(�֐��𔲂�����ŏ���)
2659             Break
2660         Case MNext%           '�p���������ꂽ�ꍇ
2661             M_20# = MClear%     '������
2662             Break
2663         Case MNgProcess%      'NG�������ꂽ�ꍇ
2664             Mov PScrewPosition(2)   '���_�Ɉړ����Ċ֐��𔲂���
2665             Break
2666         End Select
2667 *End_ScrewGet
2668     Exit Function
2669 FEnd
2670 '
2671 '��ProgramBankSet
2672 ''' <summary>
2673 ''' �˂����߂��s��(P�^�C�g)
2674 ''' </summary>
2675 '''<param name="MProgramNo">�v���O�����ԍ�</param>
2676 '''<param name="MBankNo">�o���N�ԍ�</param>
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
2697 '��fnKEY_WAIT()
2698 ''' <summary>
2699 ''' GOT����̃L�[���͑҂�
2700 ''' </summary>
2701 '''<returns>1�F��~    2�F����
2702 '''         3�F�p��    4�F�g���N�`�F�b�N�J�n
2703 '''         5�FNG
2704 '''         11�F���{�b�g�����ʒu1    12�F���{�b�g�����ʒu2
2705 '''         13�F���{�b�g�����ʒu3    14�F���{�b�g�����ʒu4
2706 '''</returns>
2707 ''' <remarks>
2708 ''' Date   : 2021/07/07 : M.Hayakawa
2709 ''' </remarks>'
2710 Function M% fnKEY_WAIT()
2711     fnKEY_WAIT = 0
2712     M_Out(MOUT_GREEN_LIGHT%) = 0        'PATLIGHT �_��
2713     M_Out(MOUT_RED_FLASH%) = 1          'PATLIGHT �ԓ_��
2714     MRtn = fnAUTO_CTL()                        'AUTO���[�h��~�A�p���L�[���͑҂�
2715     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
2716     Wait M_In(11347) = 0                'toRBT_�p���̊����҂�
2717     Dly 0.2
2718     Wait M_In(11347) = 0                'toRBT_�p���̊����҂��@2�d�m�F
2719     MLocalLoopFlg=1
2720     While MLocalLoopFlg=1
2721         If M_In(11345) = 1 Then         '��~   M5345
2722             M_Out(12343) = 1 Dly 0.5    '��~�v����M�p���X M6343
2723             fnKEY_WAIT = 1
2724             MLocalLoopFlg=-1
2725             Break
2726         ElseIf M_In(11346) = 1 Then     'fromPLC_����   M5346
2727             M_Out(12348) = 1 Dly 1.0    '���֗v����M�p���X M6348
2728             fnKEY_WAIT = 2
2729             MLocalLoopFlg=-1
2730             Break
2731         ElseIf M_In(11356) = 1 Then     'fromPLC_�p��2  M5356
2732             M_Out(12344) = 1 Dly 1.0    'toPLC_RBT�p��2�v����M M6344
2733             fnKEY_WAIT = 3
2734             MLocalLoopFlg=-1
2735             Break
2736         ElseIf M_In(11355) = 1 Then     'fromPLC_�g���N�`�F�b�N�J�n�v��
2737             M_Out(12342) = 1 Dly 0.5    'toPLC_RBT�g���N�`�F�b�N�J�n�v����M�p���X M6342
2738             fnKEY_WAIT = 4
2739             MLocalLoopFlg=-1
2740             Break
2741         ElseIf M_In(11357) = 1 Then     'fromPLC_NG�v��
2742             M_Out(12349) = 1 Dly 1.0    'toPLC_NG��M�p���X M6349
2743             fnKEY_WAIT = 5
2744             MLocalLoopFlg=-1
2745             Break
2746             '
2747         ElseIf M_In(MIN_INIT1REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu1�v�� M5568
2748             M_Out(MOUT_INIT1RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu1��M M6560
2749             fnKEY_WAIT = MRobotInit1%
2750             MLocalLoopFlg=-1
2751             Break
2752             '
2753         ElseIf M_In(MIN_INIT2REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu2�v�� M5569
2754             M_Out(MOUT_INIT2RECIVE%) = 1 Dly 0.1    'toPLC_���{�b�g�����ʒu2��M M6561
2755             fnKEY_WAIT = MRobotInit2%
2756             MLocalLoopFlg=-1
2757             Break
2758             '
2759         ElseIf M_In(MIN_INIT3REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu3�v�� M5570
2760             M_Out(MOUT_INIT3RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu3��M M6562
2761             fnKEY_WAIT = MRobotInit3%
2762             MLocalLoopFlg=-1
2763             Break
2764             '
2765         ElseIf M_In(MIN_INIT4REQUEST%) = 1 Then     'toRBT_���{�b�g�����ʒu4�v�� M5571
2766             M_Out(MOUT_INIT4RECIVE%) = 1 Dly 1.0    'toPLC_���{�b�g�����ʒu4��M M6563
2767             fnKEY_WAIT = MRobotInit4%
2768             MLocalLoopFlg=-1
2769             Break
2770             '
2771         Else
2772         EndIf
2773     WEnd
2774     M_Out(MOUT_GREEN_LIGHT%) = 1                    'PATLIGHT �_��
2775     M_Out(MOUT_RED_FLASH%) = 0                      'PATLIGHT �ԓ_��
2776     Exit Function
2777 FEnd
2778 '
2779 '�� fnAUTO_CTL
2780 ''' <summary>
2781 ''' AUTO���[�hOFF�APLC����̊J�n�҂�
2782 ''' </summary>
2783 ''' <remarks>
2784 ''' Date   : 2021/07/07 : M.Hayakawa
2785 ''' </remarks>
2786 Function M% fnAUTO_CTL
2787     fnAUTO_CTL = 0
2788     M_Out(12355) = 1            'toPLC_AUTO_MODE_OFF M6355
2789     Wait M_In(11347) = 1        'toRBT_�p���@�̎w���҂�  M5347
2790     M_Out(12355) = 0            'toPLC_AUTO_MODE_OFF M6355
2791     '
2792     If M_Svo=0 Then             '�T�[�{ON�m�F
2793         Servo On
2794     EndIf
2795     Wait M_Svo=1
2796     Exit Function
2797 FEnd
2798 '
2799 '�� fnWindScreenOpen
2800 ''' <summary>
2801 ''' �E�B���h��ʂ̕\���A��\���ݒ�
2802 ''' </summary>
2803 '''<param name="%"></param>
2804 '''<param name="%"></param>
2805 '''<param name="%"></param>
2806 '''<param name="%"></param>
2807 ''' <remarks>
2808 ''' �R�����gD1001, D1002, D1003�̐ݒ�
2809 ''' MWindReSet = 0     ��ʔ�\��
2810 ''' MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
2811 ''' MWindErrScr = 10    �G���[��� D1001, D1002
2812 ''' MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
2813 ''' Date   : 2021/07/07 : M.Hayakawa
2814 ''' </remarks>
2815 Function fnWindScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
2816     If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2817         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
2818     EndIf
2819     '
2820     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2821         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
2822     EndIf
2823     '
2824     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
2825        M_Out16(12512) = MCommentD1003            'D1003 �R�����g
2826     EndIf
2827     '
2828     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
2829     M_Out(12363) = 1                         '�E�B���h��ʐݒ�  M6362
2830     Dly 0.5
2831     M_Out(12363) = 0                         '�E�B���h��ʐݒ�
2832     Exit Function
2833 FEnd
2834 '
2835 '��FnCtlValue2
2836 ''' <summary>
2837 ''' �������A�g��OK���A�g��NG���A�z���G���[���@Read/Write
2838 ''' </summary>
2839 ''' <param name="MCtlNo%"></param>
2840 ''' <remarks>
2841 ''' Date : 2022/04/28 �n��
2842 ''' </remarks>
2843 '''
2844 '''  1�F������       �{�P
2845 '''  2�F�g���n�j��   �{�P
2846 '''  3�F�����@�Q�z���G���[�� �{�P�@�@�g��NG����ύX 2022/05/19 �n��
2847 '''  4�F�����@�P�z���G���[�� �{�P
2848 ''' 99�F�Ǐ��J�n�M�� OFF
2849 '''
2850 Function M% FnCtlValue2(ByVal MCtlNo%)
2851     FnCtlValue2 = 1
2852     Select MCtlNo%
2853         Case 1        '�������{�P
2854             M_Out(12569) = 0             '�����݊J�n�M��OFF
2855             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2856             MInputQty = M_In16(11600)    '��������M
2857             MInputQty = MInputQty + 1    '�������{�P
2858             M_Out16(12592) = MInputQty   '���������M
2859             M_Out(12569) = 1             '�����݊J�n�M��ON
2860             Break
2861             '
2862         Case 2        '�g���n�j���{�P
2863             M_Out(12569) = 0             '�����݊J�n�M��OFF
2864             M_Out(12568) = 1             '�Ǎ��݊J�n�M��ON
2865             MAssyOkQty = M_In16(11616)   '�g��OK����M
2866             MAssyOkQty = MAssyOkQty + 1  '�g��OK���{�P
2867             M_Out16(12608) = MAssyOkQty  '�g��OK�����M
2868             M_Out(12569) = 1             '�����݊J�n�M��ON
2869             Break
2870             '
2871         Case 3        '�����@�Q�z���G���[���{�P
2872             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2873             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2874             MSuctionErrQty = M_In16(11632)         '�����@�Q�z���G���[����M
2875             MSuctionErrQty = MSuctionErrQty + 1    '�����@�Q�z���G���[���{�P
2876             M_Out16(12624) = MSuctionErrQty        '�����@�Q�z���G���[�����M
2877             M_Out(12569) = 1                       '�����݊J�n�M��ON
2878             Break
2879             '
2880         Case 4        '�����@�P�z���G���[���{�P
2881             M_Out(12569) = 0                       '�����݊J�n�M��OFF
2882             M_Out(12568) = 1                       '�Ǎ��݊J�n�M��ON
2883             MSuctionErrQty = M_In16(11648)         '�����@�P�z���G���[����M
2884             MSuctionErrQty = MSuctionErrQty + 1    '�����@�P�z���G���[���{�P
2885             M_Out16(12640) = MSuctionErrQty        '�����@�P�z���G���[�����M
2886             M_Out(12569) = 1                       '�����݊J�n�M��ON
2887             Break
2888             '
2889         Case 99        '�Ǐ��J�n�M��OFF
2890             M_Out(12568) = 0        '�Ǎ��݊J�n�M��OFF
2891             M_Out(12569) = 0        '�����݊J�n�M��OFF
2892             Break
2893             '
2894     End Select
2895     Exit Function
2896 FEnd
2897 '
2898 '
2899 '��FnScreEroorCord
2900 ''' �d���h���C�o�[�̃G���[�R�[�h���܂߂��R�����g���o���ׂ̃R�����g�ԍ��̍쐬
2901 ''' �V�K�쐬�F2022/05/23 : �n��
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
2917 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2918 Function M% ISInspectionSingle( ByVal PInspPos(), ByVal MInspGrNum%(), ByVal MInspCnt%, ByVal MZAxis%, ByVal MNgContinue% )
2919 '-------------------------------------------------------------------------------
2920 'Insight�ɂ��摜�����������s�i���񏈗��Ȃ��j
2921 '   ����
2922 '       PInspPos()      �F�����ʒu
2923 '       MInspGrNum%()   �F�����ʒu�ł̌����O���[�v�ԍ��i=0�F�摜���������{�j
2924 '           PInspPos()�AMInspGrNum%()�͓����Y�����i����Step�j�̂��̂��y�A
2925 '       MInspCnt%       �F�����ʒu��
2926 '       MZAxis%         �F�I������Z���ޔ����W�i-1:�����j
2927 '                           �I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
2928 '       MNgContinue%    �F=1�Ō����G���[�ENG�������ɑSStep�̌������s��
2929 '   �߂�l�F����
2930 '       0=�ُ�I���A1=����I��
2931 '
2932 '   MInspErrNum     �F�ُ�I�����ɃG���[�ԍ����ݒ肳���
2933 '   MInspNGStepNum  �F����NG�������̌����O���[�v�ԍ����ݒ肳���
2934 '                       �����G���[�����̏ꍇ�A1��ڂ̃G���[�ԍ��A�����O���[�v�ԍ���ݒ�
2935 '   20190820    :   ���� MZAxis%,MNgContinue �ǉ�
2936 '   20200410    :   �����O���[�v�ݒ�Retry�ǉ�
2937 '-------------------------------------------------------------------------------
2938     '----- �����ݒ� -----
2939     Cnt 0                                                           '�ړ�����������(�����l=0)
2940     Fine 0.05,P                                                     '�ʒu���ߊ��������ݒu�@0.05mm
2941 '    Cnt 1,0.1,0.1
2942     '�ϐ��錾�E������
2943     Def Inte MNum                                                   '�����ԍ�(������1�`)
2944     MNum% = 1                                                       '�����ԍ������l�ݒ�
2945     Def Inte MEndFlg                                                '�����I���t���O
2946     MEndFlg% = 0
2947     '
2948     '����G�ԍ��ݒ�v���E�������s�v��off
2949     M_Out( MOUT_IS_InspGSetReq% ) = 0                               '����G�ԍ��ݒ�v��off
2950     M_Out( MOUT_IS_Insp% ) = 0                                      '�������s�v��off
2951     '�G���[�ԍ��N���A
2952     MInspErrNum = 0                                                 '�������s�G���[�ԍ�
2953     M_Out16(MOUT_InspErrNum) = MInspErrNum
2954     MInspNGStepNum = 0                                              '�������sNGStep�ԍ�
2955     M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum
2956     '
2957     'Insight Ready check?
2958     If M_In(MIN_IS_Ready) = 0 Then                                  'Ready off�Ȃ�I��
2959         MInspErrNum = 20                                            '�������s�G���[�ԍ� 20 Insight offline
2960         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2961         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2962         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2963         Exit Function
2964     EndIf
2965     '
2966     '�����ʒu���m�F
2967     If MInspCnt% < 1 Or 30 < MInspCnt% Then
2968         MInspErrNum = 21                                            '�����f�[�^�Ȃ� 21�@����<1
2969         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
2970         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
2971         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
2972         Exit Function
2973     EndIf
2974     '
2975     '
2976     '
2977     '----- ���C������ -----
2978     '�ݒ肳�ꂽ�����ʒu�����̌������s
2979     While( MEndFlg% = 0 )
2980         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ� 20200410
2981         MSetGrNumRetryExitFlg = 0
2982         MSetGrNumRetryCnt = 2                                           'Retry�񐔐ݒ�
2983         While( MSetGrNumRetryExitFlg = 0 )
2984         '----- �����O���[�v�ԍ��ݒ�Retry�ǉ������܂� 20200410
2985             '
2986             MCurrentStepErr = 0                                         '��Step�����G���[�t���O���Z�b�g
2987             '
2988             '----- �����O���[�v�ԍ��ݒ� -----
2989             M_Out16( MOUT_IS_InspGNum% ) = MInspGrNum%(MNum%)           '����G�ԍ��ݒ�
2990             M_Out( MOUT_IS_InspGSetReq% ) = 1                           '����G�ԍ��ݒ�v��on
2991             '
2992             '�����ʒu�ֈړ��E�ړ������҂�
2993             fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
2994             Mvs PInspPos( MNum% )                                       '�ړ�
2995             fnAutoScreenComment(523)                                    '��ԕ\��[�摜����������] 2022/05/09 �n��
2996             Dly 0.05                                                    '�ړ�������Delay
2997             '
2998             '�����O���[�v�ԍ��ݒ�I���m�F
2999             M_Timer(1) = 0
3000             MExitFlg = 0
3001             While( MExitFlg = 0 )
3002                 '����G�ݒ萳��I��?
3003                 If M_In( MIN_IS_InspGSetOK% ) = 1  Then
3004                     MExitFlg = 1
3005                 '
3006                 '����G�ݒ�ُ�I��?
3007                 ElseIf M_In( MIN_IS_InspGSetNG% ) = 1  Then
3008                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
3009                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
3010                         MInspErrNum = 14                                '����G�ݒ�ُ� �G���[�ԍ�=14
3011                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
3012                     EndIf
3013                     MExitFlg = 1
3014                 '
3015                 'timeout�`�F�b�N
3016                 ElseIf 1000 < M_Timer(1) Then
3017                     MCurrentStepErr = 1                                 '��Step�����G���[�t���O
3018                     If MInspErrNum = 0 Then                             '1��ڂ̃G���[?
3019                         MInspErrNum = 12                                'timeout �G���[�ԍ�=12
3020                         MInspNGStepNum = MInspGrNum%(MNum%)             '�G���[����G�ԍ��ݒ�
3021                     EndIf
3022                     MExitFlg = 1
3023                 EndIf
3024             WEnd
3025             '
3026             '����G�ԍ��ݒ�v��off
3027             M_Out( MOUT_IS_InspGSetReq% ) = 0                           '����G�ԍ��ݒ�v��off
3028             '
3029             '----- �����O���[�v�ݒ�Retry�ǉ� 20200410
3030             'NG�Ȃ���Δ�����
3031             If MCurrentStepErr = 0 Then
3032                 MSetGrNumRetryExitFlg = 1
3033             Else
3034                 'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
3035                 If MSetGrNumRetryCnt = 0 Then
3036                     MSetGrNumRetryExitFlg = 1
3037                 Else
3038                     'Retry�ց@���̑O��Delay
3039                     Dly 0.5
3040                     MSetGrNumRetryCnt = MSetGrNumRetryCnt - 1       'RetryCnt-1
3041                 EndIf
3042             EndIf
3043             '----- �����O���[�v�ݒ�Retry�ǉ������܂� 20200410
3044             '
3045         WEnd
3046         '
3047         '
3048         '
3049         '----- �������s -----
3050         If MCurrentStepErr = 0  Then                                '����G�ԍ��ݒ�NG�̏ꍇ�͌������s���Ȃ�
3051             If 0 < MInspGrNum%(MNum%) Then                          '��������?
3052                 MJudgeOKFlg = 0                                     '����OK�t���O�N���A
3053                 MInspRetryExitFlg = 0
3054                 MRetryCnt = 2                                        'Retry�񐔐ݒ�
3055                 While( MInspRetryExitFlg = 0 )
3056                     M_Out( MOUT_IS_Insp% ) = 1                      '�������s�v��on
3057                     '
3058                     '���������m�F
3059                     MRetryCnt = MRetryCnt - 1
3060                     M_Timer(1) = 0
3061                     MExitFlg = 0
3062                     While( MExitFlg = 0 )
3063                     '���������҂�
3064                         '����OK�I��?
3065                         If M_In( MIN_IS_InspOK% ) = 1  Then
3066                             MJudgeOKFlg = 1                         '����OK�t���OON
3067                             MExitFlg = 1
3068                         '
3069                         '����NG�I��?
3070                         ElseIf M_In( MIN_IS_InspNG% ) = 1  Then
3071                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
3072                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
3073                                     MInspErrNum = 32                    '����NG �G���[�ԍ�=32
3074                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
3075                                 EndIf
3076                             EndIf
3077                             MExitFlg = 1
3078                         '
3079                         '�����ُ�I��(IS timeout)?
3080                         ElseIf M_In( MIN_IS_InspErr% ) = 1  Then
3081                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
3082                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
3083                                     MInspErrNum = 38                    '�����ُ�I�� �G���[�ԍ�=38
3084                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
3085                                 EndIf
3086                             EndIf
3087                             MExitFlg = 1
3088                         '
3089                         'timeout�`�F�b�N
3090                         ElseIf 3000 < M_Timer(1) Then
3091                             If MInspErrNum = 0 Then                 '1��ڂ̃G���[?
3092                                 If MRetryCnt = 0 Then               'Retry���Ă�NG�Ȃ�NG�Ƃ���
3093                                     MInspErrNum = 34                    '�����ُ�I�� �G���[�ԍ�=34
3094                                     MInspNGStepNum = MInspGrNum%(MNum%) '�G���[����G�ԍ��ݒ�
3095                                 EndIf
3096                             EndIf
3097                             MExitFlg = 1
3098                         EndIf
3099                     WEnd
3100                     '
3101                     '�����J�n�v��off
3102                     M_Out(MOUT_IS_Insp%) = 0                        '�������s�v��off
3103                     '
3104                     'OK�Ȃ甲����
3105                     If MJudgeOKFlg = 1 Then
3106                         MInspRetryExitFlg = 1
3107                     Else
3108                         'Retry�񐔏I���ł�NG����(OK�t���Ooff�Ȃ甲����)
3109                         If MRetryCnt = 0 Then
3110                             MInspRetryExitFlg = 1
3111                         Else
3112                             'Retry�ց@���̑O��Delay
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
3123         MNum% = MNum% + 1                                           '����Step+1
3124         '�����I���m�F�@�����I���t���O�Z�b�g
3125         If (MInspCnt% < MNum% ) Then
3126             MEndFlg% = 1                                            '�����I���t���O�Z�b�g
3127         EndIf
3128         'NG���������s������
3129         If MInspErrNum <> 0 Then                                    'NG����?
3130             If MNgContinue% <> 1 Then                               'NG���s?
3131                 MEndFlg% = 1                                        '�����I���t���O�Z�b�g
3132             EndIf
3133         EndIf
3134     WEnd
3135     '
3136     '�I������Z����MZAxis�Őݒ肳�ꂽ�ʒu�܂ŏ㏸������
3137     If 0 < MZAxis% Then
3138         PCurrentPos = P_Curr                                        '���݈ʒu�擾
3139         PCurrentPos.Z = MZAxis%                                     'Z����ݒ�
3140         fnAutoScreenComment(521)                                    '��ԕ\��[�U�����{���쒆] 2022/05/09 �n��
3141         Mvs PCurrentPos                                             '���݈ʒu���ֈړ�
3142     EndIf
3143     '
3144     '�߂�l�ݒ�
3145     If MInspErrNum = 0 Or M_In(11372) = 1 Then                      '�J������������OK(M_In(11372)=1)�ǉ�(12/21����)
3146         ISInspectionSingle = 1                                      '����I���߂�l�ݒ�
3147     Else
3148         M_Out16(MOUT_InspErrNum) = MInspErrNum                      '�������s�G���[�ԍ��o��
3149         M_Out16(MOUT_InspNGStepNum) = MInspNGStepNum                '�������sNGStep�ԍ��o��
3150         ISInspectionSingle = 0                                      '�ُ�I���߂�l�ݒ�
3151     EndIf
3152     Fine 0 , P
3153     Exit Function
3154 FEnd
3155 '
3156 '��InitialZoneB
3157 ''' <summary>
3158 ''' ����~��̕��A����
3159 ''' 1)���ޔ��@Z������Ɉړ�
3160 ''' 2)J1���ȊO��ޔ��|�W�V�����ֈړ�
3161 ''' 3)J1���݂̂�ޔ��|�W�V�����ֈړ�
3162 ''' 4)�C�j�V�����|�W�V�����ֈړ�
3163 ''' </summary>
3164 ''' <remarks>
3165 ''' Date : 2022/04/12 : N.Watanabe
3166 ''' </remarks>
3167 Function V fnInitialZoneB()
3168     fnAutoScreenComment(520)    '��ԕ\��[�U�����{�����ʒu�ړ���] 2022/05/09 �n��
3169 '�p�����[�^
3170     Ovrd 5
3171 '    CmpG 0.5, 0.5, 1.0, 1.0, 1.0, 0.5, ,
3172 '    Cmp Pos, &B100011
3173 '
3174 '���A����J�n
3175 '
3176 '�u����Ɨ��݂͂̏ꏊ�́A�`���b�N���������
3177 *RecoveryChuckOpen
3178     PActive = P_Curr          '���݈ʒu���擾
3179     MRecoveryChuckOpen = 0    '�`���b�N����t���O ������
3180 'PProductOnJigSet(���i�u���ʒu)�́A�`���b�N���
3181     If (PActive.X <= PProductOnJigSet.X + 1.0) And (PActive.X >= PProductOnJigSet.X -1.0) Then
3182         If (PActive.Y <= PProductOnJigSet.Y + 1.0) And (PActive.Y >= PProductOnJigSet.Y -1.0) Then
3183             If (PActive.Z <= PProductOnJigSet.Z + 1.0) And (PActive.Z >= PProductOnJigSet.Z -1.0) Then
3184                 MRecoveryChuckOpen = 1
3185             EndIf
3186         EndIf
3187     EndIf
3188 'PProductOnJigGet(���i���o���ʒu)�́A�`���b�N���
3189     If (PActive.X <= PProductOnJigGet.X + 1.0) And (PActive.X >= PProductOnJigGet.X -1.0) Then
3190         If (PActive.Y <= PProductOnJigGet.Y + 1.0) And (PActive.Y >= PProductOnJigGet.Y -1.0) Then
3191             If (PActive.Z <= PProductOnJigGet.Z + 1.0) And (PActive.Z >= PProductOnJigGet.Z -1.0) Then
3192                 MRecoveryChuckOpen = 1
3193             EndIf
3194         EndIf
3195     EndIf
3196     If MRecoveryChuckOpen = 1 Then
3197         M_Out(12256) = 0        '�`���b�N��OFF
3198         M_Out(12257) = 1        '�`���b�N�JON
3199         M_20# = 0               'KEY���͏�����
3200         MRtn = frInCheck(11265,1,MSETTIMEOUT05&)   '�`���b�N�J���o
3201         If MRtn = 0 Then
3202             fErrorProcess(11,244,284,0)
3203             If M_20# = MNext% Then M_20# = MClear%
3204             If M_20# = MAbout% Then GoTo *RecoveryEnd
3205             If M_20# = MNgProcess% Then GoTo *RecoveryEnd
3206             If M_20# = MContinue% Then GoTo *RecoveryChuckOpen
3207         Else
3208             M_Out(12257) = 0        '�`���b�N�JOFF
3209         EndIf
3210     EndIf
3211 '
3212 '���ޔ�
3213     PActive = P_Curr
3214     Pmove = PActive
3215     Pmove.Z = 600           '���ޔ�����ꗥ�̍���
3216     If PActive.X > 400 Then
3217         Pmove.Z =400        '�p���b�g��ɘr��L�΂��Ă���Ƃ���640�܂ŏグ���Ȃ��ׁA��O���u
3218     EndIf
3219     If PActive.Z < Pmove.Z Then '���݂̍�����Pmove���Ⴂ���̂ݎ��s
3220         Mvs Pmove
3221     EndIf
3222     Dly 1.0
3223 'J1���ȊO��ޔ��|�W�V�����ֈړ�
3224     JActive = J_Curr
3225     Jmove = JTaihi
3226     Jmove.J1 = JActive.J1        'J1���̂݌��ݒl���g�p���A���̎���JTaihi�̃|�[�Y�����
3227     Mov Jmove
3228     Dly 1.0
3229 'J1���݂̂�ޔ��|�W�V�����ֈړ�
3230     Mov JTaihi
3231     Dly 1.0
3232 '�C�j�V�����|�W�V�����ֈړ�
3233     Mov PInitialPosition
3234     Cmp Off
3235     Ovrd 100
3236     M_Out(12268) = 0            '�ʒu���ߏoOFF
3237     M_Out(12269) = 1            '�ʒu���ߖ�ON
3238     fErrorProcess(11,253,281,0)
3239     Exit Function
3240 *RecoveryEnd
3241 FEnd
3242 '
3243 '
3244 '��fnAutoScreenComment
3245 ''' <summary>
3246 ''' ���C����ʂ̓���󋵕\��
3247 ''' �R�����gD1005�̐ݒ�
3248 ''' </summary>
3249 '''<param name="McommentD1005%">�R�����gID</param>
3250 ''' <remarks>
3251 ''' Date   : 2021/07/07 : M.Hayakawa
3252 ''' </remarks>
3253 Function fnAutoScreenComment(ByVal McommentD1005%)
3254     M_Out16(12576) = McommentD1005%
3255     Exit Function
3256 FEnd
3257 '
3258 '��fnRoboPosChk
3259 ''' <summary>
3260 ''' �Ō�ɏI���������{�b�g�|�W�V�����̊m�F
3261 ''' </summary>
3262 '''<param name="MINNumber%">���͔ԍ�</param>
3263 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3264 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3265 ''' PLC�ɕۑ������ԍ���Ǎ��݁A�m�F
3266 ''' MRBTOpeGroupNo = 5 �������ʒu�ɐݒ�
3267 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3268 ''' <remarks>
3269 ''' Date   : 2021/07/07 : M.Hayakawa
3270 ''' </remarks>
3271 Function M% fnRoboPosChk
3272     fnRoboPosChk = 0
3273     MRet = fnStepRead()
3274     '�����ʒu�łȂ��Ɣ��f�����ꍇ
3275     '�E�B���h��ʐ؊���
3276     If MRBTOpeGroupNo > 5 Then
3277         '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3278         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3279         Dly 0.2
3280         Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3281         Dly 1.5
3282         '
3283         fnWindScreenOpen(MWindErrScr,  64, 65, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3284         '
3285         MLoopFlg% = 1
3286         While MLoopFlg% = 1
3287             '
3288             '
3289             MKeyNumber% = fnKEY_WAIT()
3290             Select MKeyNumber%
3291                 Case Is = MAbout%       '��~
3292                     M_20# = MAbout%
3293                     MLoopFlg% = -1
3294                     Break
3295                 Case Is = MNext%        '����
3296                     'MLoopFlg% = -1
3297                     Break
3298                 Case Is = MContinue%    '�p��
3299                     M_20# = MContinue%
3300                     MLoopFlg% = -1
3301                     Break
3302                 Default
3303                     Break
3304             End Select
3305         WEnd
3306     EndIf
3307     '
3308     If M_20# = MContinue% Then                              '�p���{�^���������ꂽ�ꍇ
3309         fnWindScreenOpen(MWindInforScr,  0, 0, 34)   '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3310         Ovrd 5                                   '�ᑬ�I�[�o�[���C�h�l�ݒ�
3311         Select MRBTOpeGroupNo
3312             Case Is = 5                          '�������Ȃ�
3313                 Break
3314             Case Is = 10                         '�����ʒu�֖߂�
3315                 'Mov PTEST001
3316                 Break
3317             Case Is = 15                         '�����ʒu�֖߂�
3318                 'Mov PTEST002
3319                 Dly 0.5
3320                 'Mov PTEST001
3321                 Dly 0.5
3322                 Break
3323             Default
3324                 Break
3325         End Select
3326         '
3327         Ovrd M_NOvrd                            '�V�X�e���̏����l��ݒ�
3328         M_Out(12364) = 1                        'toPLC_�f�[�^�ۑ�ON
3329         MRBTOpeGroupNo = 5
3330         MRet = fnStepWrite(MRBTOpeGroupNo)      '�����ʒu�̔ԍ��]��
3331         Dly 1.0
3332         M_Out(12364) = 0                        'toPLC_�f�[�^�ۑ�OFF
3333         fnRoboPosChk = 1                        '�����ʒu������s
3334         fnWindScreenOpen(MWindReSet,  0, 0, 10)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3335     EndIf
3336     Exit Function
3337 FEnd
3338 '
3339 '��frInCheck
3340 ''' <summary>
3341 ''' �Z���T�[IN�`�F�b�N
3342 ''' </summary>
3343 '''<param name="MINNumber%">���͔ԍ�</param>
3344 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3345 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3346 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
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
3365 '��frScrewInCheck
3366 ''' <summary>
3367 ''' �˂��z���Z���T�[IN�`�F�b�N
3368 ''' </summary>
3369 '''<param name="MINNumber%">���͔ԍ�</param>
3370 '''<param name="MCMPFLG%">0:OFF�m�F 1:ON�m�F</param>
3371 '''<param name="MTimeCnt&">�^�C���A�E�g����</param>
3372 '''<returns>���� 0:�^�C���A�E�g 1:OK</returns>
3373 ''' <remarks>
3374 ''' Date   : 2022/07/10 : �����V��
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
3396 '�˂����ߋ@�ʐM�m�F
3397 '
3398 '-----------------------------------------------
3399 Function M% fScewTcomChk
3400     fScewTcomChk = 0
3401     '�ʐM�m�F���M
3402     M_Out(MOUT_ScwT_ComChk%) = MOn%
3403     '�ʐM�m�F��M�ҋ@
3404     Wait M_In(MIN_ScwT_comOK%) = MOn%
3405     '�ʐM�m�F���M�I��
3406     M_Out(MOUT_ScwT_ComChk%) = MOff%
3407     Exit Function
3408 FEnd
3409 '
3410 '
3411 '-----------------------------------------------
3412 '
3413 '�˂����ߊJ�n���M
3414 '
3415 '-----------------------------------------------
3416 Function M% fScewTStart
3417     fScewTStart = 0
3418     '�˂����ߊJ�n�ҋ@����M
3419     Wait M_In(MIN_ScwT_STRec%) = MOn%
3420     Dly 0.1
3421     '�˂����ߊJ�n��M�𑗐M
3422     M_Out(MOUT_ScwT_ST%) = MOn% Dly 0.5 '0.5msec�p���X
3423     Exit Function
3424 FEnd
3425 '
3426 '
3427 '-----------------------------------------------
3428 '
3429 '�˂����ߊ�����M
3430 '
3431 '-----------------------------------------------
3432 Function M% fScewTFinish
3433     fScewTFinish = 0
3434     '�˂����ߊ����ҋ@����M
3435     Wait M_In(MIN_ScwT_Fin%) = MOn%
3436     Dly 0.1
3437     '�˂����ߊ�����M�𑗐M
3438     M_Out(MOUT_ScwT_FinOK%) = MOn% Dly 0.5  '0.5msec�p���X
3439     Exit Function
3440 FEnd
3441 '
3442 '
3443 '-----------------------------------------------
3444 '
3445 '����xx��~��M
3446 '
3447 '-----------------------------------------------
3448 Function M% fScewTCaseStop(ByVal MCase%())
3449     fScewTCaseStop = 0
3450     '����xx��~����M
3451     Wait M_In(MCase%(1)) = MOn%
3452     Dly 0.1
3453     '����xx��~��M�𑗐M
3454     M_Out(MCase%(2)) = MOn% Dly 0.5 ' 0.5msec�p���X
3455     Exit Function
3456 FEnd
3457 '
3458 '-----------------------------------------------
3459 '
3460 '�ĊJ�n��M
3461 '
3462 '-----------------------------------------------
3463 Function M% fScewTReStart()
3464     fScewTReStart = 0
3465     '�ĊJ�n����M
3466     Wait M_In(MIN_ScwT_ReST%) = MOn%
3467     Dly 0.1
3468     '�ĊJ�n��M�𑗐M
3469     M_Out(MOUT_ScwT_ReSTOK%) = MOn% Dly 0.5 '0.5msec�p���X
3470     Exit Function
3471 FEnd
3472 '
3473 '��fErrorProcess
3474 '<summary>
3475 '�G���[����
3476 '</summary>
3477 '<param name = "MErrorScreenNo%"> �X�N���[���ԍ�</param>
3478 '<param name = "MErrorCommentD1001%"> D1001�R�����g�ԍ� </param>
3479 '<param name = "MErrorCommentD1002%"> D1002�R�����g�ԍ� </param>
3480 '<param name = "MErrorCommentD1003%"> D1003�R�����g�ԍ� </param>
3481 '<make>
3482 '2021/11/5 �����V��
3483 '</make>
3484 Function V fErrorProcess(ByVal MErrorScreenNo% , ByVal MErrorCommentD1001% , ByVal MErrorCommentD1002% , ByVal MErrorComentD1003)
3485     MScreenNo = MErrorScreenNo%                    '�G���[�X�N���[���ԍ�
3486     MCommentD1001 = MErrorCommentD1001%            'D1001�R�����g�ԍ�
3487     MCommentD1002 = MErrorCommentD1002%            'D1002�R�����g�ԍ�
3488     MCommentD1003 = MErrorCommentD1003%            'D1003�R�����g�ԍ�
3489 *RETRY_ERR_PROCESS
3490      M_20# = MClear%     '������
3491 '        '�G���[�����L�q
3492         fnWindScreenOpen(MScreenNo , MCommentD1001, MCommentD1002, MCommentD1003)
3493 '        'GOT KEY���͑҂�
3494         MKeyNumber = fnKEY_WAIT()
3495 '        '
3496         If MKeyNumber = MAbout% Then   '��~��I�������ꍇ
3497             M_20# = MAbout%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3498             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3499             Break
3500          '
3501         ElseIf MKeyNumber = MContinue% Then   '�p����I�������ꍇ
3502             M_20# = MContinue%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3503             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3504         '
3505         ElseIf MKeyNumber = MNext% Then   '���ւ�I�������ꍇ
3506             M_20# = MNext%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3507             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3508          '
3509         ElseIf MKeyNumber = MNgProcess% Then   '��~��I�������ꍇ
3510             M_20# = MNgProcess%            'M_20# �v���O�����ԋ��ʊO���ϐ�
3511             fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3512             Break
3513         '
3514         EndIf
3515         '
3516         If M_20# = MClear% Then *RETRY_ERR_PROCESS
3517     Exit Function
3518 FEnd
3519 '
3520 '��fnTorqueCheck
3521 ''' <summary>
3522 ''' �g���N�`�F�b�N����p�̃��C��
3523 ''' </summary>
3524 ''' <remarks>
3525 ''' Date   : 2021/12/21 : H.AJI
3526 ''' </remarks>'
3527 Function M% fnTorqueCheck
3528     '�g���N�`�F�b�N�����M  �����n��~
3529     M_Out(MOUT_TORQUE_CHECK%) = 1     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3530     '
3531     fnTorqueCheck = 0
3532     Ovrd 20
3533     Mov PInitialPosition              '�����ʒu�ړ�
3534     Ovrd 100
3535     '���L�L�[�҂��̌p���ɔ��������Ȃ�����
3536     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂�
3537     Dly 0.2
3538     Wait M_In(11347) = 0                 'toRBT_�p���̊����҂��@2�d�m�F
3539     '
3540     'M6340  �g���N�`�F�b�N��M
3541     'Dly 5.0
3542     M_Out(12340) = 1          '�g���N�`�F�b�N��M M6340
3543     Dly 1.0
3544     M_Out(12340) = 0
3545     '
3546     MRet = fnMainScreenOpen(11, 60, 61, 0)   '�g���N�`�F�b�N��ʕ\��
3547     '
3548     MLoopFlg = 1
3549     While MLoopFlg = 1
3550         '
3551         Mov PInitialPosition              '�����ʒu�ړ�
3552         '
3553         MKeyNumber = fnKEY_WAIT()
3554         Select MKeyNumber
3555             Case Is = 1           '��~
3556                 M_Out(12343) = 1          '��~�v���J�n�v����M M6343
3557                 Dly 1.0
3558                 M_Out(12343) = 0
3559                 Ovrd 20
3560                 Mov PTicketRead_1
3561                 Ovrd 100
3562                 M_20# = 1
3563                 MLoopFlg = -1
3564                 Break
3565             Case Is = 2           '����
3566                 Break
3567             Case Is = 3           '�p��
3568                 Break
3569             Case Is = 4           '�g���N�`�F�b�N�J�n
3570                 M_Out(12545) = 1    ' toPLC_PC�g���N�`�F�b�N1�v����M(M315)
3571                 M_Out(12342) = 1 Dly 1.0    '�g���N�`�F�b�N�J�n�v����M M6342
3572                 fnWindScreenOpen(29,  0, 0, 0)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3573                 'MRet = fnWindScreenOpen(MWindInfoScr,  0, 0, 67)  '�E�B���h��ʃG���[�\���ƃR�����g�ݒ�
3574                 MRet = fnMoveTorquePosi()
3575                 'MRet = fnAutoScreenComment(67)  'AUTO��� �ʉߗ���NG������
3576                 'MRet = fnWindScreenOpen(MWindReSet, 0, 0, 0)  '�G���[��ʏ���
3577                 Break
3578             Default
3579                 Break
3580         End Select
3581     WEnd
3582     '
3583     '�g���N�`�F�b�N����~���M
3584     M_Out(MOUT_TORQUE_CHECK%) = 0     ' 12367  'PLC�փg���N�`�F�b�N���𑗐M
3585     '
3586     '���{�b�g�̈ʒu�����ɖ߂�
3587     '
3588     Exit Function
3589  FEnd
3590  '
3591 '
3592 '
3593 '---------------------------
3594 '
3595 '    ���C����ʂ̕\���A��\���ݒ�
3596 '         �R�����gD1001, D1002, D1003�̐ݒ�
3597 '           MWindReSet = 0     ��ʔ�\��
3598 '           MWindInfoScr = 5   �C���t�H���[�V������� D1003�̂�
3599 '           MWindErrScr = 10    �G���[��� D1001, D1002
3600 '           MWindCmmnScr = 20   �G���[�ȊO�̃R�����g��� D1001, D1002
3601 '
3602 '---------------------------
3603 Function M% fnMainScreenOpen(ByVal MScreenNo,  ByVal MCommentD1001, ByVal MCommentD1002, ByVal MCommentD1003)
3604     fnMainScreenOpen = 0
3605     '
3606    If MCommentD1001 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3607         M_Out16(12480) = MCommentD1001            'D1001 �R�����g
3608     EndIf
3609     '
3610     If MCommentD1002 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3611         M_Out16(12496) = MCommentD1002            'D1002 �R�����g
3612     EndIf
3613     '
3614     If MCommentD1003 <> 0 Then                    '�R�����g 0 �͐ݒ肪�Ȃ��̂Ŋm�F
3615         M_Out16(12512) = MCommentD1003            'D1003 �R�����g
3616     EndIf
3617     '
3618     M_Out16(12448) = MScreenNo                '��ʔԍ�  M6448   10=�G���[���
3619     M_Out(12362) = 1                         '�E�B���h��ʐݒ�  M6362
3620     Dly 0.5
3621     M_Out(12362) = 0                         '�E�B���h��ʐݒ�
3622     Exit Function
3623 FEnd
3624 '
3625 '��Main
3626 ''' <summary>
3627 ''' �g���N�`�F�b�N������
3628 ''' </summary>
3629 ''' <remarks>
3630 ''' Date   : 2021/12/21 : H.AJI
3631 ''' </remarks>'
3632 Function M% fnMoveTorquePosi
3633      fnMoveTorquePosi = 0
3634      Ovrd 50
3635      Mov PTorqueCheck_1 '�g���N�`�F�b�N���[�^�[���ֈړ�
3636     '
3637     Spd M_NSpd
3638 '-------------      �h���C�o�[RST
3639     M_Out(12240)=0     '�h���C�o�[OFF CCW
3640     M_Out(12241)=0     '�h���C�o�[OFF CW
3641     M_Out(12242)=0     '�h���C�o�[���� C1
3642     M_Out(12243)=0     '�h���C�o�[���� C2
3643     M_Out(12245)=0     '�v���O�������� F1/�v���O����2
3644 '---------------------------------------
3645 '[P-11]
3646 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�z
3647     Mov PTorqueCheck, -50                     ' �g���N-1�@�u���ʒu��� 50mm �ֈړ�
3648     Dly 0.1
3649 '-----------------------
3650    'Cnt 0                           'Cnt����-2�@�I��
3651 '-----------------------
3652     Mov PTorqueCheck , -5                      '�g���N-1�@�u���ʒu��� 5mm �ֈړ�
3653     Dly 0.2
3654 '-----------------------
3655     ProgramBankSet(1,3)
3656     M_Out(12241)=0                   '�h���C�o�[OFF  CW
3657     'Dly 0.1
3658 '--------------------------------
3659     Ovrd 40
3660    'Dly 0.1
3661 '--------------------------------  �l�W���ߑ��x�ݒ�
3662     Spd 14                            '���C�h 100-40 100% :Spd 12
3663     Dly 0.1
3664 '--------------------------------
3665 '--------------------------------
3666 '---------------------------------�y�˂����ߓ���z
3667 '
3668     'Mvs PTorquePosi020 WthIf M_In(11584)=1,Skip  '�ړ����G���[���o
3669    Mvs PTorqueCheck               '�g���N�`�F�b�N�ʒu�ֈړ�
3670     Dly 0.3                          '�������҂�
3671    M_Out(12241)=1                   '�h���C�o�[ON  CW
3672 '
3673     Wait M_In(11584)=1                '����/�G���[���o
3674     Dly 0.1
3675     Spd M_NSpd
3676    'Ovrd 20
3677     If M_In(11256)=1 Then *LBL1       '�l�W�g�[�^���G���[���o
3678     Wait M_In(11257)=1                '�l�W����SC
3679 '---------------------------------
3680     Dly 0.1
3681     M_Out(12241)=0                    '�h���C�o�[OFF CW
3682     Dly 0.1
3683     M_Out(12242)=0                    '�h���C�o�[���� C1
3684     Dly 0.1
3685     M_Out(12243)=0                    '�h���C�o�[���� C2 (�o���N3)
3686     Dly 0.1
3687     M_Out(12245)=0                    '�v���O����2���� F1
3688 '--------------------------------------------------------------   �y�g���N�`�F�b�N 0.4N - P11�����܂Łz
3689 '
3690     Mvs PTorqueCheck,-60                       '������mov ����ύX
3691     Dly 0.1
3692 '--------------------------------------------------------------
3693    'Ovrd 80
3694 '--------------------------------------------------------------
3695 '---------------------------------------
3696 '---------------------------------------
3697 '---------------------------------------�G���[���E����
3698    *LBL1
3699    Fsc Off            '�͊o�Z���T�@Off   *STEP1�͕s�v
3700    Mvs ,-100
3701    M_Out(12241)=0     '�h���C�o�[OFF CW
3702    Dly 0.1
3703    M_Out(12242)=0     '�h���C�o�[���� C1
3704    Dly 0.1
3705    M_Out(12243)=0     '�h���C�o�[���� C2 (�o���N3)
3706    Dly 0.1
3707    M_Out(12245)=0     '�v���O�������� F1
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
3718 '��Main
3719 ''' <summary>
3720 ''' �g������p�̃��C��
3721 ''' </summary>
3722 ''' <remarks>
3723 ''' Date   : 2021/07/07 : M.Hayakawa
3724 ''' </remarks>'
3725 Function Main
3726     MopeNo = M_21#         '�O���ϐ��ɂē���ԍ����
3727     '
3728     If M_Svo=0 Then
3729         Servo On
3730     EndIf
3731     Wait M_Svo=1
3732 '�g���X�^�[�g���t�����v���p���XON
3733     M_Out(MOUT_ST_DATETIME%) = 1 Dly 0.5
3734 '�p�g���C�g����
3735     M_Out(MOUT_PATLIGHT_ON%) = 1                'PATLIGHT���쌠ON
3736     M_Out(MOUT_GREEN_LIGHT%) = 1                'PATLIGHT ��
3737     '
3738     M_20# = 0                                   'KEY���͏�����
3739     M_Out(MOUT_OKNG%) = 0                       '��H����NG�t���O���o�͏�����
3740     MRet% = 0
3741 '�����ʒu�̊m�F�ƈړ�
3742 '
3743 '���A����@���s�E�����s����      2022/04/12 �n�� �쐬
3744     PActive = P_Curr                    '���݈ʒu���擾
3745     MRecoveryPass% = 0
3746     If (PActive.X <= PInitialPosition.X + 1.0) And (PActive.X >= PInitialPosition.X -1.0) Then
3747         If (PActive.Y <= PInitialPosition.Y + 1.0) And (PActive.Y >= PInitialPosition.Y -1.0) Then
3748             If (PActive.Z <= PInitialPosition.Z + 1.0) And (PActive.Z >= PInitialPosition.Z -1.0) Then
3749                 MRecoveryPass% = 1       '�C�j�V�����|�W�V�����͕��A����p�X
3750             EndIf
3751         EndIf
3752     EndIf
3753     If (PActive.X <= PTicketRead_1.X + 1.0) And (PActive.X >= PTicketRead_1.X -1.0) Then
3754         If (PActive.Y <= PTicketRead_1.Y + 1.0) And (PActive.Y >= PTicketRead_1.Y -1.0) Then
3755             If (PActive.Z <= PTicketRead_1.Z + 1.0) And (PActive.Z >= PTicketRead_1.Z -1.0) Then
3756                 MRecoveryPass% = 1       '�`�P�b�g�ǂݍ��ݏ��ʒu�͕��A����p�X
3757             EndIf
3758         EndIf
3759     EndIf
3760     If MRecoveryPass% = 0 Then
3761        fnInitialZoneB()        '���A����p�X�t���O�������Ă��Ȃ����͕��A��������s
3762     EndIf
3763 '
3764 '
3765 '    MRet% = fnRoboPosChk()
3766     If MRet% = 1 Then                           '�����ʒu�̓�����s�����ꍇ
3767         fnWindScreenOpen(MWindCmmnScr,  70, 71, 0)  '�E�B���h���
3768         MKeyNumber% = fnKEY_WAIT()
3769         Select MKeyNumber%
3770             Case Is = MAbout%       '��~
3771                 M_20# = MAbout%
3772                 MLoopFlg% = -1
3773                 Break
3774             Case Is = MNext%        '����
3775                 'MLoopFlg = -1
3776                 Break
3777             Case Is = MContinue%    '�p��
3778                 M_20# = MContinue%
3779                 MLoopFlg% = -1
3780                 Break
3781             Default
3782                 Break
3783         End Select
3784     EndIf
3785     '
3786     If M_20# <> MAbout% Then        '�O���ϐ� M_20# �� 1=��~ �ȊO�̏ꍇ
3787         M_Out(12364) = 1            'toPLC_�f�[�^�ۑ�ON
3788 '�g���N�`�F�b�N
3789         If MopeNo = 2 Or M_In(MIN_TorqueCheck%) = 1 Then
3790             MRet% = fnTorqueCheck()
3791             Break
3792         Else
3793 '            If M_In(MIN_Insight_Use%) = 1 Then  'toRBT_�g�p�m�F
3794 '                MRtn = InspInit()               '�摜��������������
3795 '            EndIf
3796             '
3797            M_20# = MClear%                    '������
3798 '�g���J�n
3799             If M_In(MIN_ASSY_CANCEL%) = 0 Then
3800 '                MRet% = fnAssyStart()
3801                 fnAssyStart()
3802             Else
3803                 M_20# = MPass%
3804             EndIf
3805 '�g���I�����t����
3806             M_Out(MOUT_ED_DATETIME%) = 1    '�g���I�����t����
3807             Wait M_In(11572) = 1            '���t�擾����
3808             Dly 0.1
3809             M_Out(MOUT_ED_DATETIME%) = 0    '�g���I�����t����
3810 '���t�^�[���j�b�g�ւ�OUT
3811             '  KEY���͂������Ȃ��ꍇ OK�Ɣ��f
3812             fnAutoScreenComment(89)         'AUTO��� �g����������
3813             'MRet% = fnWindScreenOpen(MWindReSet, 0, 0, 89)  'AUTO��� �g����������
3814 'OK/NG�t���O�o��
3815             If M_20# <= 0 Then
3816                 M_Out(MOUT_OKNG%) = 1       '��H����OK�t���O���o��(PLC OUT)
3817             ElseIf M_20# = MPass% Then
3818                 M_Out(MOUT_OKNG%) = 0       '��H����NG�t���O���o��(PLC OUT)
3819             EndIf
3820 'PIAS�ɑg������������
3821             If M_In(MIN_PIAS_Use%) = 1 Then       'PIAS_ON�m�F
3822                 If M_20# = MPass% Then
3823                     M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3824                 Else
3825                     'KEY���͂�NG�̏ꍇ
3826                     If M_20# = MNgProcess% Then
3827                         M_Out(MOUT_OKNG%) = 0                   '��H����NG�t���O���o��(PLC OUT)
3828                         fnAutoScreenComment(90)  'AUTO��� �ʉߗ���NG������
3829                         MRet% = fnPiasWrite(MNG%)
3830                        nAssyNgQty = nAssyNgQty + 1
3831                     EndIf
3832                     '
3833                     'KEY���͂������Ȃ��ꍇ OK�Ɣ��f(MAssyOK%�ɕύX1/17����)
3834                     If M_20# = MAssyOK% Then
3835                             '-----------------------
3836                             'D732 -> D2600 �R�s�[�v��
3837                             M_Out(12566) = 1
3838 '                            Wait M_In(11581) = 1   'PLC���R�s�[�����M��
3839                             M_Out(12566) = 0
3840                             '
3841                         If M_In(11367) = 0 Then          '����������݃L�����Z��=1 DEbug�p
3842                             'MRet% = fnAutoScreenComment(91)  'AUTO��� ���񏑍���
3843                             '��ԍ��ƍ�(PP�͖��g�p�j
3844 '                            MRet% = fnPCBNumberCheck()
3845                         Else
3846                             MRet% = 1
3847                         EndIf
3848                         '
3849                         If M_In(11368) = 0 Then          '�H�����������݃L�����Z��=1 DEbug�p
3850                             If M_20# <> MAbout% Then
3851                                 '�H������OK��������
3852                                 M_Out(MOUT_OKNG%) = 1                   '��H����OK�t���O���o��(PLC OUT)
3853                                 fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3854                                 MRet% = fnPiasWrite(MOK%)
3855                                 nAssyOkQty = 0
3856                                 nAssyOkQty = nAssyOkQty + 1
3857                             Else
3858                                 nAssyOkQty = nAssyOkQty + 1
3859                             EndIf
3860                         EndIf
3861                     EndIf
3862 '                    fnAutoScreenComment(92)  'AUTO��� �ʉߗ���OK������
3863 '                    MRet% = fnPiasWrite(MOK%)
3864                 EndIf
3865             Else
3866                 nAssyOkQty = nAssyOkQty + 1
3867             EndIf
3868             '
3869             '�g���I�����t��������
3870             M_Out(MOUT_ED_DATETIME%) = 0                '�g���I�����t����
3871             '�������A�g��OK���A�g��NG��������
3872 '            MRtn = FnCtlValue2(2)                       '������ 2022/04/28 �R�����g�A�E�g �n��
3873             '
3874 '            If M_In(MIN_Insight_Use%) = 1 Then          'toRBT_�g�p�m�F
3875 '                '�摜�����I������
3876 '                MRtn = InspQuit()
3877 '            EndIf
3878         EndIf
3879         M_Out(12364) = 0                          'toPLC_�f�[�^�ۑ�OFF
3880     EndIf
3881 '�p�g���C�g����
3882     M_Out(MOUT_PATLIGHT_ON%) = 0                  'PATLIGHT���쌠ON
3883     M_Out(MOUT_GREEN_LIGHT%) = 0                  'PATLIGHT ��
3884 'GOT�\��
3885     fnAutoScreenComment(93)  'AUTO��� �H������
3886 FEnd
3887 End
3888 '
3889 '���܂��Ȃ��R�����g
3890 '��΍폜�����
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
