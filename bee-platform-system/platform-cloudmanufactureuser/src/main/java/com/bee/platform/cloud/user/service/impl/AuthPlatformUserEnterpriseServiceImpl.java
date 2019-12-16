package com.bee.platform.cloud.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.user.dao.mapper.AuthPlatformUserEnterpriseMapper;
import com.bee.platform.cloud.user.dto.UserEnterpriseDTO;
import com.bee.platform.cloud.user.entity.AuthEnterprise;
import com.bee.platform.cloud.user.entity.AuthFactory;
import com.bee.platform.cloud.user.entity.AuthPlatformUserEnterprise;
import com.bee.platform.cloud.user.service.AuthEnterpriseService;
import com.bee.platform.cloud.user.service.AuthFactoryService;
import com.bee.platform.cloud.user.service.AuthPlatformUserEnterpriseService;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 企业与用户中间表 服务实现类
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthPlatformUserEnterpriseServiceImpl extends ServiceImpl<AuthPlatformUserEnterpriseMapper,
        AuthPlatformUserEnterprise> implements AuthPlatformUserEnterpriseService {


    @Autowired
    private AuthPlatformUserEnterpriseMapper authPlatformUserEnterpriseMapper;

    @Autowired
    private AuthEnterpriseService authEnterpriseService;

    @Autowired
    private AuthFactoryService authFactoryService;

    /**
     * @notes: 通过用户id查询用户的企业信息
     * @Author: junyang.li
     * @Date: 11:01 2019/9/19
     * @param userId : 用户id
     * @return: com.bee.platform.cloud.user.entity.AuthPlatformUserEnterprise
     */
    @Override
    public UserEnterpriseDTO selectEnterpriseByUserId(Integer userId) {
        //通过用户id查询用户信息
        AuthPlatformUserEnterprise userEnterprise=this.selectOne(new EntityWrapper<AuthPlatformUserEnterprise>()
                .where("deleted=0 and status=1 and user_id={0}",userId).last("limit 0,1"));
        //判空
        if(userEnterprise==null){
            log.error("用户加入任何企业，无法登录。用户id是:{}",userId);
            throw new BusinessException(ResCodeEnum.NO_AUTHORITY, ExceptionMessageEnum.NO_AUTHORITY);
        }
        //复制数据
        UserEnterpriseDTO dto= BeanUtils.copyProperties(userEnterprise,UserEnterpriseDTO.class);
        //查询企业名称
        AuthEnterprise authEnterprise=authEnterpriseService.selectById(userEnterprise.getEnterpriseId());
        dto.setEnterpriseName(authEnterprise.getName());
        //查询工厂信息
        AuthFactory authFactory=authFactoryService.selectById(userEnterprise.getFactoryId());
        if(authFactory!=null){
           dto.setFactoryName(authFactory.getFactoryName());
        }
        return dto;
    }
}
