package com.bee.platform.cloud.si.manufacture.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.VersionUpgradeMapper;
import com.bee.platform.cloud.si.manufacture.dto.VersionUpgradeDTO;
import com.bee.platform.cloud.si.manufacture.entity.VersionUpgrade;
import com.bee.platform.cloud.si.manufacture.service.VersionUpgradeService;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * <p>
 * 版本升级描述 服务实现类
 * </p>
 *
 * @author LL123
 * @since 2019-11-25
 */
@Service
public class VersionUpgradeServiceImpl extends ServiceImpl<VersionUpgradeMapper, VersionUpgrade> implements VersionUpgradeService {


    @Override
    public ResponseResult<List<VersionUpgradeDTO>> getByVersionNum(String versionNum) {
        List<VersionUpgrade> list = this.selectList(new EntityWrapper<>(new VersionUpgrade().setVersionNum(versionNum)));
        if (CollectionUtils.isEmpty(list)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>());
        }
        HashMap<String, VersionUpgrade> map = Maps.newHashMap();
        list.forEach(a -> {
            String num = a.getVersionNum();
            VersionUpgrade dto = map.get(num);

            if (dto == null) {
                map.put(num, new VersionUpgrade()
                        .setVersionNum(num)
                        .setDescription(a.getDescription())
                        .setUpgradeTime(a.getUpgradeTime()));
            } else {
                dto.setDescription(dto.getDescription() + "\n\r" + a.getDescription());
            }
        });
        ArrayList<VersionUpgrade> list1 = new ArrayList<>(map.values());
        list1.sort((o1, o2) -> o2.getUpgradeTime().compareTo(o1.getUpgradeTime()));

        DateFormat df = new SimpleDateFormat("yyyy-MM-dd");
        List<VersionUpgradeDTO> resultList = Lists.newArrayList();
        list1.forEach(a -> resultList.add(
                new VersionUpgradeDTO()
                        .setUpgradeTime(df.format(a.getUpgradeTime()))
                        .setDescription(a.getDescription())
                        .setVersionNum(a.getVersionNum())));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resultList);
    }
}
