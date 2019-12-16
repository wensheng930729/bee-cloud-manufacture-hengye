package com.bee.platform.cloud.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.bcrypt.BCryptPassword;
import com.bee.platform.cloud.user.dto.UserEnterpriseDTO;
import com.bee.platform.cloud.user.dto.UserRoleParamDTO;
import com.bee.platform.cloud.user.entity.AuthPlatformUser;
import com.bee.platform.cloud.user.rq.AuthUserRQ;
import com.bee.platform.cloud.user.rq.CodeLoginRQ;
import com.bee.platform.cloud.user.service.*;
import com.bee.platform.common.constants.enums.PlatformType;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.service.SystemCodeService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.DateUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.RandomStringUtils;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @description: 登录
 * @author: junyang.li
 * @create: 2019-03-04 16:54
 **/
@Slf4j
@Service
public class AuthUserLoginServiceImpl implements AuthUserLoginService {

    @Autowired
    private ConfigService configService;

    @Autowired
    private SystemCodeService systemCodeService;

    @Autowired
    private AuthPlatformUserService userService;

    @Autowired
    private JedisService jedisService;

    @Autowired
    private AuthPlatformUserEnterpriseService authPlatformUserEnterpriseService;

    @Autowired
    private AuthUserRoleService authUserRoleService;

    @Autowired
    private AuthResourceService authResourceService;
    /**
     * @notes 登录
     * @Author junyang.li
     * @Date 16:56 2019/3/4
     **/
    @Override
    public ResponseResult<AuthPlatformUserInfo> login(AuthUserRQ authUserRQ,PlatformType type) {
        //测试账号是否允许登录
        if(!testAccountLogin(authUserRQ.getUsername())){
            log.info("测试账号登陆开关关闭，不允许测试账号登陆username={}",authUserRQ.getUsername());
            return ResponseResult.buildResponseResult(ResCodeEnum.TEST_ACCOUNT_CANNOT_LOGIN,null);
        }
        AuthPlatformUser user = userService.getUserByUsername(authUserRQ.getUsername());
        //验证密码
        if(Objects.isNull(user)){
            log.info("登录验证失败，数据库无法查询到相应的信息，用户账号是:{}",authUserRQ.getUsername());
            return ResponseResult.buildResponseResult(ResCodeEnum.ACCOUNT_NOT_FOUND,null);
        }
        //账号被禁用
        if (Status.FALSE.getKey().equals(user.getStatus())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PROHIBIT_ACCOUNT);
        }
        if(!BCryptPassword.matches(authUserRQ.getPassword(),user.getPassword())){
            log.info("登录验证失败，账号或密码错误，登录账号是:{}",authUserRQ.getUsername());
            return ResponseResult.buildResponseResult(ResCodeEnum.LOGIN_FAIL,null);
        }
        user.setCurrentClientId(authUserRQ.getCurrentClientId());
        //单点登录
        return this.getSysToken(user,type);
    }
    /**
     * @notes: app端账号密码登录
     * @Author: junyang.li
     * @Date: 20:00 2019/9/24
     * @param authUserRQ :
     * @return: com.bee.platform.common.entity.AppUserInfo
     */
    @Override
    public ResponseResult<AppUserInfo> clientLogin(AuthUserRQ authUserRQ) {
        return this.appLoginCommon(this.login(authUserRQ,PlatformType.CLOUD_MAF_APP));
    }

    /**
     * @notes 短信验证码登录
     * @Author junyang.li
     * @Date 10:41 2019/3/14
     **/
    @Override
    public ResponseResult<AuthPlatformUserInfo> codeLogin(CodeLoginRQ rq) {
        //测试账号是否允许登录
        String username=rq.getUsername();
        if(!testAccountLogin(username)){
            log.info("测试账号登陆开关关闭，不允许测试账号登陆username={}",username);
            return ResponseResult.buildResponseResult(ResCodeEnum.TEST_ACCOUNT_CANNOT_LOGIN,null);
        }
        AuthPlatformUser user = userService.getUserByUsername(username);
        //验证密码
        if(Objects.isNull(user)){
            log.info("登录验证失败，数据库无法查询到相应的信息，用户账号是:{}",username);
            return ResponseResult.buildResponseResult(ResCodeEnum.ACCOUNT_NOT_FOUND,null);
        }
        //账号被禁用
        if (Status.FALSE.getKey().equals(user.getStatus())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PROHIBIT_ACCOUNT);
        }
        //输入的验证码
        String code=rq.getCode();
        //验证验证码
        String oldCode=jedisService.get(ConstantsUtil.VALIDATE_CODE+username);
        //为空则过期
        if (StringUtils.isEmpty(oldCode)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.VALIDATE_CODE_EXPIRE);
        }
        if (!code.equals(oldCode)) {
            log.info("验证不通过,短信验证码不正确。输入的code={}，缓存中的code={}",code,oldCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.VALIDATE_CODE_ERROR);
        }
        //删除session中的验证码
        jedisService.delKey(ConstantsUtil.VALIDATE_CODE+username);
        user.setCurrentClientId(rq.getCurrentClientId());
        //单点登录
        return this.getSysToken(user,PlatformType.CLOUD_MAF_WEB);
    }
    /**
     * @notes: app端短信验证码登录
     * @Author: junyang.li
     * @Date: 14:10 2019/9/25
     * @param rq : 传入参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.AuthPlatformUserInfo>
     */
    @Override
    public ResponseResult<AppUserInfo> appCodeLogin(CodeLoginRQ rq) {
        return this.appLoginCommon(this.codeLogin(rq));
    }
    /**
     * @notes: APP端账号密码登录和验证码登录的公共方法
     * @Author: junyang.li
     * @Date: 14:13 2019/9/25
     * @param result : 登录成功后返回的结果
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.AppUserInfo>
     */
     private ResponseResult<AppUserInfo> appLoginCommon(ResponseResult<AuthPlatformUserInfo> result){
        if(!ResCodeEnum.SUCCESS.getCode().equals(result.getCode())){
            return ResponseResult.buildResponseResult(result.getCode(),result.getMessage());
        }
        AuthPlatformUserInfo userInfo=result.getObject();
        AuthRoleInfo authRoleInfo=userInfo.getRoleInfo();
        //登录成功获取所有的菜单信息
        List<AuthResourceInfo> list=authResourceService.getResourceByRoleId(authRoleInfo.getRoleId(),
                PlatformType.CLOUD_MAF_APP.getValue());
        if(CollectionUtils.isEmpty(list)){
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_AUTHORIZE);
        }
        AppUserInfo appUserInfo=new AppUserInfo(userInfo,list);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,appUserInfo);
    }
    /**
     * @notes: 单点登录
     * @Author: junyang.li
     * @Date: 10:23 2019/9/19
     * @param user : 当前用户
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.AuthPlatformUserInfo>
     */
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<AuthPlatformUserInfo> getSysToken(AuthPlatformUser user, PlatformType type){
        String key=ConstantsUtil.USER_TOKEN_GROUP+user.getUsername();
        //根据不同的客户端下发不同的token
        String oldToken=jedisService.getHashToJsonStr(key,type.getValue());
        //单点登录
        if(!StringUtils.isEmpty(oldToken)){
            //则判断缓存中是否存在用户信息
            if(jedisService.exists(oldToken)){
                //存在则删除
                jedisService.delKey(oldToken);
            }
        }
        //重新生成用户信息
        String sysToken=this.createSysToken(type);
        //查询token的失效时间
        int expireSeconds = Integer.valueOf(configService.getConfValue(ConstantsUtil.TOKEN_EXPIRES_IN,
                "7200", "Token 在redis在的失效时间"));
        Date expireDate= DateTime.now().plusSeconds(expireSeconds).toDate();
        //复制数据
        AuthPlatformUserInfo info= BeanUtils.copyProperties(user,AuthPlatformUserInfo.class);
        //查询用户当前的企业和工厂
        UserEnterpriseDTO dto=authPlatformUserEnterpriseService.selectEnterpriseByUserId(user.getId());
        info.setOrgId(dto.getEnterpriseId())
                .setOrg_name(dto.getEnterpriseName())
                .setSysToken(sysToken)
                .setExpiresIn(expireDate)
                .setFactoryId(dto.getFactoryId())
                .setFactoryName(dto.getFactoryName())
                .setExpiresTime(expireSeconds);
        AuthPlatformUser now=new AuthPlatformUser().setId(user.getId())
                .setCurrentEnterpriseId(dto.getEnterpriseId())
                .setSysToken(sysToken)
                .setExpiresIn(expireDate)
                .setCurrentClientId(user.getCurrentClientId())
                .setUpdateTime(new Date());
        UserRoleParamDTO param=new UserRoleParamDTO(user.getId(),dto.getEnterpriseId());
        //查询当前用户的角色
        AuthRoleInfo authUserRole=authUserRoleService.getRoleByUserId(param);
        //用户角色信息
        info.setRoleInfo(authUserRole);
        //将用户信息更新至数据库
        userService.updateById(now);
        //将用户 信息插入缓存中
        jedisService.setObject(info.getSysToken(),info,expireSeconds);
        //将不同平台的token也放入缓存中
        jedisService.setHash(key,Collections.singletonMap(type.getValue(),sysToken),
                expireSeconds);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,info);
    }

    /**
     * @notes  是否允许测试账号登录
     * @Author junyang.li
     * @Date 10:46 2019/3/14
     **/
    private boolean testAccountLogin(String username){
        //测试账号是否允许登陆的开关
        String val=configService.getConfValue(ConstantsUtil.TEST_ACCOUNT_LOGIN,"1",
                "是否允许测试账号登陆的开关。0不允许，1允许");
        //测试账号允许登陆
        if(Status.TRUE.getKey().toString().equals(val)){
            return true;
        }
        //获取测试账号列表
        List<SystemCode> list=systemCodeService.getCacheSysCodeInfo(ConstantsUtil.ALL_TEST_ACCOUNT_KEY, ConstantsUtil.TEST_ACCOUNT);
        if(CollectionUtils.isEmpty(list)){
            return true;
        }
        List<String> sysCodes=list.stream().map(SystemCode::getSysCode).collect(Collectors.toList());
        //包含则返回false不允许登录
        return !sysCodes.contains(username);
    }

    /**
     * @notes 注销登录
     * @Author junyang.li
     * @Date 9:50 2019/3/5
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> logout(String sysToken) {
        AuthPlatformUserInfo userInfo= userService.getSelfInfo(sysToken);
        if(userInfo==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        //将对应数据库的sysToken设置失效
        Integer loginCount = jedisService.getJsonObject(ConstantsUtil.LOGIN_COUNT_KEY + sysToken, Integer.class);
        if (Objects.isNull(loginCount) || loginCount.equals(1)) {
            jedisService.delKey(ConstantsUtil.LOGIN_COUNT_KEY + sysToken);
            userService.update(new AuthPlatformUser().setExpiresIn(new Date()),
                    new EntityWrapper<>(new AuthPlatformUser().setSysToken(sysToken)));
            jedisService.delKey(sysToken);
        } else {
            loginCount = loginCount -1;
            if (loginCount > 0) {
                int seconds = jedisService.ttl(sysToken, TimeUnit.SECONDS).intValue();
                jedisService.setJsonObject(ConstantsUtil.LOGIN_COUNT_KEY + sysToken, loginCount, seconds);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes: 创建登录凭证
     * @Author: junyang.li
     * @Date: 17:13 2019/9/18
     * @return: java.lang.String
     */
    private   String createSysToken(PlatformType type){
        String uuid= UUID.randomUUID().toString();
        return type.getToken()+ConstantsUtil.LINE+uuid+
                ConstantsUtil.LINE+RandomStringUtils.randomAlphanumeric(6);
    }
}
