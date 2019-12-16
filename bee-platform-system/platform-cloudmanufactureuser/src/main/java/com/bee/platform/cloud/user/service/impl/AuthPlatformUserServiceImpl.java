package com.bee.platform.cloud.user.service.impl;

import com.aliyuncs.exceptions.ClientException;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.bcrypt.BCryptPassword;
import com.bee.platform.cloud.user.dao.mapper.AuthPlatformUserEnterpriseMapper;
import com.bee.platform.cloud.user.dao.mapper.AuthPlatformUserMapper;
import com.bee.platform.cloud.user.dto.CustomerAccountDTO;
import com.bee.platform.cloud.user.dto.UserDetailedDTO;
import com.bee.platform.cloud.user.dto.UserRoleParamDTO;
import com.bee.platform.cloud.user.entity.*;
import com.bee.platform.cloud.user.rq.AddUserRQ;
import com.bee.platform.cloud.user.rq.EditPasswordRQ;
import com.bee.platform.cloud.user.rq.EditUserRQ;
import com.bee.platform.cloud.user.rq.UpdatePaaswordRQ;
import com.bee.platform.cloud.user.service.*;
import com.bee.platform.cloud.user.sms.SmsService;
import com.bee.platform.common.constants.enums.EnumState;
import com.bee.platform.common.constants.enums.PlatformType;
import com.bee.platform.common.constants.enums.RoleType;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.enums.UserType;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.*;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author xin.huang
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthPlatformUserServiceImpl extends ServiceImpl<AuthPlatformUserMapper, AuthPlatformUser>
        implements AuthPlatformUserService {

    @Autowired
    private AuthPlatformUserMapper authPlatformUserMapper;
    @Autowired
    private SmsService smsService;
    @Autowired
    private JedisService jedisService;
    @Autowired
    private AuthRoleService authRoleService;
    @Autowired
    private AuthUserRoleService authUserRoleService;
    @Autowired
    private AuthPlatformUserEnterpriseService userEnterpriseService;
    @Autowired
    private AuthPlatformUserEnterpriseMapper userEnterpriseMapper;
    @Autowired
    private AuthResourceService authResourceService;
    @Autowired
    private AuthEnterpriseService authEnterpriseService;

    @Value("${aliyun.oss.smsAddCustomer}")
    private String smsAddCustomer;

    /**
     * @notes: 通过用户id查询用户信息
     * @Author: junyang.li
     * @Date: 16:44 2019/9/25
     * @param userId : 用户id
     * @return: com.bee.platform.cloud.user.entity.AuthPlatformUser
     */
    @Override
    public AuthPlatformUser getUserById(Integer userId) {
       if(userId!=null){
           return authPlatformUserMapper.selectOne(new AuthPlatformUser().setId(userId)
                   .setDeleted(Status.FALSE.getKey()));
       }
       return null;
    }
    /**
     * @notes: 通过用户账号查询用户信息
     * @Author: junyang.li
     * @Date: 16:44 2019/9/25
     * @param username : 用户账号
     * @return: com.bee.platform.cloud.user.entity.AuthPlatformUser
     */
    @Override
    public AuthPlatformUser getUserByUsername(String username) {
        if(!StringUtils.isEmpty(username)){
            return authPlatformUserMapper.selectOne(new AuthPlatformUser().setUsername(username)
                    .setDeleted(Status.FALSE.getKey()));
        }
        return null;
    }

    /**
     * @Description 内部系统获取用户信息
     * @Param sysToken
     * @Date 2019/5/24 11:36
     * @Author xin.huang
     * @Return
     */
    @Override
    public AuthPlatformUserInfo getUserInfo(String sysToken) {
        if (StringUtils.isBlank(sysToken)) {
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO, ExceptionMessageEnum.USER_NOT_LEGAL);
        }
        AuthPlatformUserInfo userInfo = getUserInfoToRedis(sysToken);
        if (userInfo == null) {
            log.info("getUserInfo()方法中登录凭证有误无法从缓存中拿到用户信息sysToken={}", sysToken);
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO, ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        //未切换企业
        if (userInfo.getRoleInfo()==null) {
            log.error("用户在企业下没有角色，sysToken={}。没有权限：{}", sysToken, userInfo);
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO, ExceptionMessageEnum.USER_NOT_AUTHORIZE);
        }
        return userInfo;
    }

    /**
     * @Description 通过token获得用户信息
     * @Param sysToken
     * @Date 2019/5/24 11:41
     * @Author xin.huang
     * @Return
     */
    @Override
    public AuthPlatformUserInfo getSelfInfo(String sysToken) {
        return this.getUserInfo(sysToken);
    }

    /**
     * @Description 用于存在的用户请求验证码
     * @Param phone
     * @Date 2019/5/24 11:48
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ResCodeEnum> sendMessage(String phone) {
        AuthPlatformUser user = this.getUserByUsername(phone);
        if (user == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ACCOUNT_NOT_FOUND);
        }
        try {
            return smsService.sendMessage(phone);
        } catch (ClientException e) {
            log.error("用户请求验证码失败，用户手机号是：{}，异常信息是：{}", phone, e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_SYSTEM);
    }
    /**
     * @notes: 用户修改自己的密码
     * @Author: junyang.li
     * @Date: 14:45 2019/10/9
     * @param rq :
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updatePassword(EditPasswordRQ rq) {
        //缓存校验结果
        boolean result = smsService.getCheckResult(rq.getUsername(),rq.getCode());
        if (!result) {
            throw new BusinessException(ResCodeEnum.NOT_VALIDATE, ExceptionMessageEnum.NOT_VALIDATE);
        }
        //用户是否存在
        AuthPlatformUser user = authPlatformUserMapper.selectOne(new AuthPlatformUser()
                .setUsername(rq.getUsername()).setDeleted(Status.FALSE.getKey()));
        //数据有误
        if (Objects.isNull(user)) {
            throw new BusinessException(ResCodeEnum.USER_NOT_EXIST, ExceptionMessageEnum.USER_NOT_EXIST);
        }
        //新密码加密
        String newPassword=BCryptPassword.encode(rq.getNewPassword());
        //是否与旧密码相同
        if (BCryptPassword.matches(newPassword, user.getPassword())) {
            throw new BusinessException(ResCodeEnum.OLD_PASSWORD_SAME_NEW, ExceptionMessageEnum.OLD_PASSWORD_SAME_NEW);
        }
        AuthPlatformUser newUser = new AuthPlatformUser().setId(user.getId())
                .setPassword(newPassword)
                .setUpdateTime(new Date())
                .setExpiresIn(new Date());
        //清除用户缓存
        if(jedisService.exists(user.getSysToken())){
            jedisService.delKey(user.getSysToken());
        }
        //修改新密码
        authPlatformUserMapper.updateById(newUser);
        jedisService.delKey(ConstantsUtil.VALIDATE_RESULT);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes: 新增用户
     * @Author: junyang.li
     * @Date: 14:25 2019/9/20
     * @param rq : 新增参数
     * @param userInfo : 当前操作人
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> addUser(AuthPlatformUserInfo userInfo,AddUserRQ rq) {
        //查询用户是否已经存在
        AuthPlatformUser authPlatformUser=this.getUserByUsername(rq.getUsername());
        //不为空，则表示已存在
        if(authPlatformUser!=null){
            log.info("新增用户，用户已经存在。username={}",rq.getUsername());
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_ALREADY_EXIST);
        }
        //查询角色是否存在
        AuthRole authRole=authRoleService.selectOne(new EntityWrapper<AuthRole>()
                .where("deleted=0 and id={0}",rq.getRoleId()));
        if(authRole==null){
            log.info("修改用户信息时，角色不存在。roleId={}",rq.getRoleId());
            return ResponseResult.buildResponseResult(ResCodeEnum.ROLE_NOT_FOUND);
        }
        // 判断用户是否有权限选择该角色
        boolean result = this.authChangeRole(userInfo,authRole);
        if(!result){
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_SELECT_ROLE);
        }
        AuthPlatformUser newUser=new AuthPlatformUser()
                .setPhone(CommonUtils.disposePhoneNum(rq.getUsername()))
                .setBeesrvId(Long.toString(IdUtil.nextId()))
                .setUsername(rq.getUsername())
                .setName(rq.getName())
                .setUserType(UserType.PLAIN_USER.getKey())
                .setActiveType(Status.TRUE.getKey())
                .setNickname(rq.getName())
                .setCreateTime(new Date())
                .setDeleted(Status.FALSE.getKey())
                .setPassword(BCryptPassword.encode(rq.getPassword()))
                .setUpdateTime(new Date())
                .setStatus(Status.TRUE.getKey());
        //新增用户
        authPlatformUserMapper.insert(newUser);
        //查出用户id
        newUser=authPlatformUserMapper.selectOne(new AuthPlatformUser()
                .setUsername(rq.getUsername())
                .setDeleted(Status.FALSE.getKey()));
        //添加用户企业和工厂
        AuthPlatformUserEnterprise userEnterprise=new AuthPlatformUserEnterprise()
                .setUserId(newUser.getId())
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setCreateTime(new Date())
                .setCreateUser(userInfo.getId())
                .setUpdateTime(new Date())
                .setStatus(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey());
        //添加用户角色
        AuthUserRole userRole=new AuthUserRole()
                .setRoleId(rq.getRoleId())
                .setUserId(newUser.getId())
                .setEnterpriseId(userInfo.getOrgId())
                .setRoleType(authRole.getRoleType())
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey())
                .setCreateTime(new Date())
                .setCreateUser(userInfo.getId())
                .setUpdateTime(new Date());
        authUserRoleService.insert(userRole);
        userEnterpriseService.insert(userEnterprise);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * @notes: 判断当前用户是否有权限选择该角色
     * @Author: junyang.li
     * @Date: 17:35 2019/10/31
     * @param userInfo : 当前操作人
     * @param authRole :
     * @return: boolean
     */
    private boolean authChangeRole(AuthPlatformUserInfo userInfo,AuthRole authRole){
        // 如果新增的是企业管理员，则判断该用户是否是该企业的管理员
        String userRoleType=userInfo.getRoleInfo().getRoleType();
        //超级管理员权限直接返回true
        if(RoleType.SUPER_ADMIN.getCode().equals(userRoleType)){
            return true;
        }
        if(RoleType.isAdmin(authRole.getRoleType())){
            //当前用户非企业管理员
            if(!RoleType.isAdmin(userRoleType)){
                log.error("当前用户非企业管理员，并且将其他用户修改成管理员,用户没有权限。当前用户角色是={}", userRoleType);
                return false;
            }
        }else {
            // 否则判断是否是当前的企业角色
            if(!userInfo.getOrgId().equals(authRole.getEnterpriseId())){
                log.info("修改用户信息时，没有权限选择该非本企业角色。" +
                                "当前用户企业id是：{},角色所属企业是:{}",
                        userInfo.getOrgId(),authRole.getEnterpriseId());
                return false;
            }
        }
        return true;
    }

    /**
     * @notes: 工厂配置 - 人员管理 - 用户列表查询
     * @Author: junyang.li
     * @Date: 14:45 2019/9/25
     * @param keyword : 搜索关键词
     * @param roleId : 角色id
     * @param pagination : 分页对象
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.cloud.user.dto.UserDetailedDTO>>
     */
    @Override
    public ResponseResult<List<UserDetailedDTO>> getUserByKeyWord(AuthPlatformUserInfo userInfo,
                                                                  String keyword, Integer roleId,
                                                                  Pagination pagination) {
        //如果角色id不为空，查询角色是否存在
        AuthRole authRole=null;
        if(roleId!=null){
            authRole=authRoleService.selectById(roleId,userInfo.getOrgId());
            if(authRole==null){
                return ResponseResult.buildResponseResult(ResCodeEnum.ROLE_NOT_FOUND);
            }
        }
        List<UserDetailedDTO> dtos=authPlatformUserMapper.selectUserByKeyword(keyword,userInfo.getOrgId(),roleId,pagination);
        //查询角色
        if(CollectionUtils.isEmpty(dtos)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new ArrayList<>(),
                    PageUtils.transToPage(pagination));
        }
        //判断角色是否为空
        if(roleId!=null){
            for (UserDetailedDTO obj:dtos) {
                obj.setRoleName(authRole.getRoleName())
                .setStatus(EnumState.getValue(obj.getState()))
                .setState(null);
                //超级管理员不允许编辑
                if(RoleType.isAdmin(authRole.getRoleType())){
                    obj.setCanEdit(Status.FALSE.getKey());
                }
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dtos,
                    PageUtils.transToPage(pagination));
        }
        //否则获得所有角色的map对象
        Map<Integer,AuthRole> map=authRoleService.getAllRoleToMap();
        dtos.forEach(obj->{
            AuthRole role=map.get(obj.getRoleId());
            obj.setRoleName(role.getRoleName())
                    .setStatus(EnumState.getValue(obj.getState()))
                    .setState(null);
            //超级管理员不允许编辑
            if(RoleType.isAdmin(role.getRoleType())){
                obj.setCanEdit(Status.FALSE.getKey());
            }else {
                obj.setCanEdit(Status.TRUE.getKey());
            }
        });
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dtos,
                PageUtils.transToPage(pagination));
    }
    /**
     * @notes: 工厂配置中 - 启用或禁用账号
     * @Author: junyang.li
     * @Date: 15:42 2019/9/25
     * @param userInfo : 当前操作人
     * @param userId : 被操作人id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateUserStatus(AuthPlatformUserInfo userInfo, Integer userId) {
        //判断禁用的是否是自己
        if(userInfo.getId().equals(userId)){
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PROHIBIT_OWN);
        }
        //用户是否存在
        AuthPlatformUser authPlatformUser=this.getUserById(userId);
        //判空
        if(authPlatformUser==null){
            log.info("启用禁用账号的方法中无法通过用户id查询到用户信息。用户id是:{}",userId);
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        //查询该用户角色
        AuthRoleInfo authRoleInfo=authUserRoleService.getRoleByUserId(new UserRoleParamDTO().setUserId(userId));
        //判空
        if(authRoleInfo!=null){
            if(RoleType.isAdmin(authRoleInfo.getRoleType())){
                return ResponseResult.buildResponseResult(ResCodeEnum.NOT_UPDATE_ADMIN);
            }
        }
        //否则修改该用户状态
        int status=Status.TRUE.getKey().equals(authPlatformUser.getStatus())?Status.FALSE.getKey():Status.TRUE.getKey();
        this.updateById(new AuthPlatformUser()
                .setId(userId)
                .setStatus(status)
                .setUpdateUser(userInfo.getId())
                .setUpdateTime(new Date()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * @notes: 工厂配置，修改员工信息
     * @Author: junyang.li
     * @Date: 16:42 2019/9/25
     * @param userInfo : 当前操作人
     * @param rq : 被修改用户信息
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateUser(AuthPlatformUserInfo userInfo, EditUserRQ rq) {
        //查询用户是否存在
        AuthPlatformUser authPlatformUser=this.getUserById(rq.getUserId());
        //判空
        if(authPlatformUser==null){
            log.info("修改用户信息时，无法通过用户id查询到用户信息。userId={}",rq.getUserId());
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        //查询角色是否存在
        AuthRole authRole=authRoleService.selectOne(new EntityWrapper<AuthRole>()
                .where("deleted=0 and id={0}",rq.getRoleId()));
        if(authRole==null){
            log.info("修改用户信息时，角色不存在。roleId={}",rq.getRoleId());
            return ResponseResult.buildResponseResult(ResCodeEnum.ROLE_NOT_FOUND);
        }
        // 判断用户是否有权限选择该角色
        boolean result = this.authChangeRole(userInfo,authRole);
        if(!result){
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_SELECT_ROLE);
        }
        //修改用户信息
        AuthPlatformUser user=new AuthPlatformUser().setId(rq.getUserId())
                .setName(rq.getName())
                .setNickname(rq.getName())
                .setPhone(rq.getUsername())
                .setUpdateUser(userInfo.getId())
                .setUpdateTime(new Date());

        //判断用户密码是否为空
        if(!StringUtils.isEmpty(rq.getPassword())){
            String newPassword=BCryptPassword.encode(rq.getPassword());
            user.setPassword(newPassword);
        }
        //修改用户角色
        authUserRoleService.update(new AuthUserRole().setRoleId(authRole.getId()).setRoleType(authRole.getRoleType()),
                new EntityWrapper<AuthUserRole>().where("deleted=0 and user_id={0}",rq.getUserId()));
        this.updateById(user);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes: app 端通过token获得用户信息
     * @Author: junyang.li
     * @Date: 14:32 2019/9/26
     * @param sysToken : 当前用户token
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.AppUserInfo>
     */
    @Override
    public ResponseResult<AppUserInfo> getSelfInfoByApp(String sysToken) {
        AuthPlatformUserInfo userInfo=this.getUserInfo(sysToken);
        AuthRoleInfo authRoleInfo=userInfo.getRoleInfo();
        //登录成功获取所有的菜单信息
        List<AuthResourceInfo> list=authResourceService.getResourceByRoleId(authRoleInfo.getRoleId(), PlatformType.CLOUD_MAF_APP.getValue());
        AppUserInfo appUserInfo=new AppUserInfo(userInfo,list);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,appUserInfo);
    }

    /**
     * @notes: 后台用户密码
     * @Author: junyang.li
     * @Date: 17:31 2019/9/25
     * @param userInfo : 当前操作人
     * @param rq : 新密码参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateUserPassword(AuthPlatformUserInfo userInfo, UpdatePaaswordRQ rq) {
        //查询用户是否存在
        AuthPlatformUser authPlatformUser=this.getUserById(rq.getUserId());
        //判空
        if(authPlatformUser==null){
            log.info("修改用户信息时，无法通过用户id查询到用户信息。userId={}",rq.getUserId());
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        //查询用户角色，超级管理员不允许被修改
        AuthRoleInfo authRoleInfo=authUserRoleService.getRoleByUserId(new UserRoleParamDTO().setUserId(rq.getUserId()));
        if(RoleType.isAdmin(authRoleInfo.getRoleType())){
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_UPDATE_ADMIN);
        }
        //修改用户密码
        String newPassword=BCryptPassword.encode(rq.getPassword());
        this.updateById(new AuthPlatformUser().setId(authPlatformUser.getId())
                .setPassword(newPassword).setUpdateUser(userInfo.getId())
                .setUpdateTime(new Date()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes: 新增客户账号
     * @Author: junyang.li
     * @Date: 17:31 2019/9/25
     * @param userInfo : 当前操作人
     * @param dto : 新增账号信息
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> addCustomerAccount(AuthPlatformUserInfo userInfo, CustomerAccountDTO dto) {
        //查询账号是否存在
        AuthPlatformUser user=this.selectOne(new EntityWrapper<AuthPlatformUser>()
                .where("status=1 and deleted=0 and username={0}",dto.getUsername()));
        if(user!=null){
            log.info("账号已经存在，无需新增:{}",dto);
            return ResponseResult.buildResponseResult(ResCodeEnum.ACCOUNT_EXIST);
        }
        //校验企业是否存在
        AuthEnterprise enterprise=authEnterpriseService.selectById(dto.getEnterpriseId());
        if(enterprise==null){
            log.info("企业不存在，无法新增用户",dto);
            throw new BusinessException(ResCodeEnum.ENTERPRISE_NOT_EXIST,
                    ExceptionMessageEnum.ENTERPRISE_NOT_EXIST);
        }
        //随机生成6位密码 暂时写死
        String password="123456";//RandomStringUtils.randomAlphanumeric(6);
        log.info("当前用户密码是:{}",password);
        //加密
        String encode=BCryptPassword.encode(password);
        //不存在则新增
        user=new AuthPlatformUser().setUsername(dto.getUsername())
                .setName(dto.getName())
                .setNickname(dto.getName())
                .setPassword(encode)
                .setPhone(CommonUtils.disposePhoneNum(dto.getUsername()))
                .setActiveType(Status.TRUE.getKey())
                .setCreateTime(new Date())
                .setUpdateTime(new Date())
                .setDeleted(Status.FALSE.getKey())
                .setUserType(UserType.CUSTOMER.getKey());
        this.insert(user);
        //新增用户和企业关联关系
        AuthPlatformUserEnterprise enterpriseUser=new AuthPlatformUserEnterprise()
                .setUserId(user.getId())
                .setCreateUser(userInfo.getId())
                .setEnterpriseId(dto.getEnterpriseId())
                .setCreateTime(new Date())
                .setStatus(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey());
        userEnterpriseService.insert(enterpriseUser);
        AuthRole authRole=this.getCustomerRole(dto);
        //添加用户角色
        AuthUserRole userRole=new AuthUserRole()
                .setRoleId(authRole.getId())
                .setRoleType(authRole.getRoleType())
                .setUserId(user.getId())
                .setEnterpriseId(dto.getEnterpriseId())
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey())
                .setCreateTime(new Date())
                .setCreateUser(userInfo.getId())
                .setUpdateTime(new Date());
        authUserRoleService.insert(userRole);
        //向该用户发送密码短信
        Map<String,Object> map=new HashMap<>(16);
        map.put("account",dto.getUsername());
        map.put("password",password);
        map.put("download","app-download/bee.apk");
        return smsService.sendMessageForPrompt(dto.getUsername(),smsAddCustomer,map);
        //return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * @notes: 根据用户id查询用户详细
     * @Author: junyang.li
     * @Date: 13:45 2019/10/31
     * @param enterpriseId : 当前操作人企业id
     * @param userId : 待查询用户id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.user.entity.AuthPlatformUser>
     */
    @Override
    public ResponseResult<AuthPlatformUserInfo> getUserInfoById(Integer enterpriseId, Integer userId) {
        //查询用户的企业信息
        AuthPlatformUserEnterprise userEnterprise=userEnterpriseService.selectOne(new EntityWrapper<AuthPlatformUserEnterprise>()
                .where("status =1 and deleted =0 and user_id={0} and enterprise_id={1}",userId,enterpriseId));
        if(userEnterprise==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_THE_ENTERPRISE_USER);
        }
        //查询用户信息
        AuthPlatformUser platformUser=this.getUserById(userId);
        if(platformUser==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                BeanUtils.copyProperties(platformUser,AuthPlatformUserInfo.class));
    }
    /**
     * @notes: 内部系统通过用户id获得用户信息,返回一个map对象
     * @Author: junyang.li
     * @Date: 11:40 2019/11/6
     * @param enterpriseId :
     * @param userIds : 用户id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.AuthPlatformUserInfo>
     */
    @Override
    public ResponseResult<Map<Integer, AuthPlatformUserInfo>> getSimpleUsers(Integer enterpriseId,List<Integer> userIds) {
        //判空
        if(enterpriseId==null || CollectionUtils.isEmpty(userIds)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new HashMap<>(1));
        }
        //查询在该企业下的用户
        List<Integer> currentUserIds=userEnterpriseMapper.findUserByEnterpriseId(enterpriseId,userIds);
        if(CollectionUtils.isEmpty(currentUserIds)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new HashMap<>(1));
        }
        //查询这些用户的用户信息
        List<AuthPlatformUserInfo> list= authPlatformUserMapper.selectUserByIds(currentUserIds);
        if(CollectionUtils.isEmpty(list)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new HashMap<>(1));
        }
        Map<Integer, AuthPlatformUserInfo> map=new HashMap<>(16);
        list.forEach(obj->map.put(obj.getId(),obj));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,map);
    }

    /**
     * @notes: 判断客户的类型并且返回相应的角色，现在需求不明确，后期更改
     * @Author: junyang.li
     * @Date: 14:58 2019/10/24
     * @param dto :
     * @return: com.bee.platform.cloud.user.entity.AuthRole
     */
    private AuthRole getCustomerRole(CustomerAccountDTO dto){
        RoleType type=Status.TRUE.getKey().equals(dto.getCarrier())?RoleType.CARRIER:RoleType.CUSTOMER;
        return authRoleService.selectOne(new EntityWrapper<AuthRole>()
                .where("deleted=0 and role_type={0}",type.getCode()));
    }
    /**
     * @notes: 将用户信息放入缓存中
     * @Author: junyang.li
     * @Date: 17:02 2019/9/18
     * @param sysToken : 登录凭证
     * @return: com.bee.platform.common.entity.AuthPlatformUserInfo
     */
    private AuthPlatformUserInfo getUserInfoToRedis(String sysToken) {
        try {
            //获得该对象
            return jedisService.getJsonObject(sysToken, AuthPlatformUserInfo.class);
        } catch (Exception e) {
            log.info("redis缓存连接异常，异常信息是:{}", e);
            AuthPlatformUser user = authPlatformUserMapper.selectOne(new AuthPlatformUser()
                    .setSysToken(sysToken).setDeleted(Status.FALSE.getKey()));
            if (Objects.isNull(user)) {
                log.info("从数据库中查询的用户信息为空。sysToken={}", sysToken);
                return null;
            }
            return BeanUtils.copyProperties(user, AuthPlatformUserInfo.class)
                    .setExpiresIn(user.getExpiresIn())
                    .setSysToken(user.getSysToken()).setOrgId(user.getCurrentEnterpriseId());
        }
    }

    /**
     * @param list : 用户集合
     * @notes: 批量插入用户
     * @Author: junyang.li
     * @Date: 10:51 2019/5/24
     * @return: void
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void insertAllUser(List<AuthPlatformUser> list) {
        if (!CollectionUtils.isEmpty(list)) {
            authPlatformUserMapper.insertAllUser(list);
        }
    }
}
