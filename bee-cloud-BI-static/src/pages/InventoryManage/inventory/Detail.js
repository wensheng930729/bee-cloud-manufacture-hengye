
import { PureComponent } from 'react'
import { Table, Row, Col, Card, message } from 'antd'
import { getDetail } from '../services/inventoryService'

export default class Detail extends PureComponent {
  state = {
    confirmModalVisible: false,
    data: {}
  }

  componentDidMount() {
    const { location } = this.props;
    this.inventoryOrderId = location.query.inventoryOrderId;
    if (!this.inventoryOrderId) {
      return message.info('盘点单号不正确')
    }

    //获取详情数据
    getDetail(this.inventoryOrderId).then(res => {
      if (res.code === 1) {
        this.setState({
          data: res.object
        })
      }
    })
  }

  render() {

    const { data: { createTime, creator, remarks, list = [] } } = this.state;

    const columns = [
      {
        title: '产品名',
        dataIndex: 'productName',
      },
      {
        title: '仓库',
        dataIndex: 'storageName',
      },
      {
        title: '计量单位',
        dataIndex: 'productUnit',
      },
      {
        title: '账面数量',
        dataIndex: 'accountNum',
      },
      {
        title: '实盘数量',
        dataIndex: 'actualNum',
      },
      {
        title: '差异数量',
        dataIndex: 'differenceNum',
      }
    ];

    return (
      <div style={{ marginLeft: 32, marginRight: 32 }}>
        <Card>
          <Row style={{ lineHeight: '50px' }}>
            <Col span={12}>
              <Row>
                <Col style={{ textAlign: 'right' }} span={8}>盘点单单号：</Col>
                <Col span={16}>{this.inventoryOrderId}</Col>
              </Row>
            </Col>

            <Col span={12}>
              <Row>
                <Col span={8} style={{ textAlign: 'right' }}  >盘点日期：</Col>
                <Col span={16}>{createTime}</Col>
              </Row>
            </Col>
          </Row>
          <Row style={{ lineHeight: '50px' }}>
            <Col span={12}>
              <Row>
                <Col span={8} style={{ textAlign: 'right' }} >盘点人：</Col>
                <Col span={16}>{creator}</Col>
              </Row>
            </Col>
            <Col span={12}>
              <Row>
                <Col span={8} style={{ textAlign: 'right' }} >备注：</Col>
                <Col span={16}>{remarks}</Col>
              </Row>
            </Col>
          </Row>
        </Card>

        <Card style={{ marginTop: 10 }}>
          <Table dataSource={list} columns={columns} pagination={false} />
        </Card>
      </div>
    );
  }
};
