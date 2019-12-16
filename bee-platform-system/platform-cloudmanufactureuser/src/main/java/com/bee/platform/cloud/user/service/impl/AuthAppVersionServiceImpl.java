package com.bee.platform.cloud.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.cloud.user.dto.AppVersionDTO;
import com.bee.platform.cloud.user.entity.AuthAppVersion;
import com.bee.platform.cloud.user.dao.mapper.AuthAppVersionMapper;
import com.bee.platform.cloud.user.service.AuthAppVersionService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.BeanUtils;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-12
 */
@Service
public class AuthAppVersionServiceImpl extends ServiceImpl<AuthAppVersionMapper, AuthAppVersion> implements AuthAppVersionService {

    /**
     * @notes: 获取app最新的版本号
     * @Author: junyang.li
     * @Date: 9:14 2019/9/30
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.user.dto.AppVersionDTO>
     */
    @Override
    public ResponseResult<List<AppVersionDTO>> getVersion() {
        List<AuthAppVersion> list=this.selectList(new EntityWrapper<>());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                BeanUtils.assemble(AppVersionDTO.class,list));
    }
    /**
     * @notes: 获得最新的app地址，不包含域名
     * @Author: junyang.li
     * @Date: 18:11 2019/10/24
     * @return: java.lang.String
     */
    @Override
    public String getDownload() {
        return null;
    }
}
