package com.bee.platform.cloud.si.manufacture.dao.mapper;

import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.cloud.user.Application;
import com.bee.platform.cloud.user.entity.ArUser;
import com.bee.platform.cloud.user.service.ArUserService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@RunWith(SpringRunner.class)
@SpringBootTest(classes={Application.class})
public class ArUserMapperTest {

	@Autowired
	private ArUserService arUserService;
	
	@Test
	public void testSelectList() {
		List<ArUser> list = arUserService.selectList(new EntityWrapper<ArUser>());
		log.info("list={}", list);
	}
	
}
