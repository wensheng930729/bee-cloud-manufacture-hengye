package com.bee.platform.cloud.user.service.impl;

import com.bee.platform.cloud.user.entity.AuthEnterprise;
import com.bee.platform.cloud.user.dao.mapper.AuthEnterpriseMapper;
import com.bee.platform.cloud.user.service.AuthEnterpriseService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

/**
 * <p>
 * 企业表 服务实现类
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-19
 */
@Service
public class AuthEnterpriseServiceImpl extends ServiceImpl<AuthEnterpriseMapper, AuthEnterprise> implements AuthEnterpriseService {

}
